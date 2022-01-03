#' Need to standardize crop type before the curation process
#' This function is based on create_newdbnames() and create_newdbnames_db()
create_new_db_entries <- function(auxiliary_files, filename, matches_temp_list, data_intid, match_type){
  names_standardized <- read.csv(here(auxiliary_files, filename), stringsAsFactors = FALSE) %>%
    mutate(var_id = as.character(var_id))
  
  # This merges the standardized names back together with the variety names and associated information
  if(!("fuzzy" %in% names(matches_temp_list))){ 
    warning("matches_temp_list must be provided a list element named 'fuzzy' containing results of fuzzymatching")}
  
  
  # Need to decide which set of data to join to
  # The goal is to reinstate any supplementary columns 
  # A secondary goal is to check that the number of rows in names_standardized is the 
  # same as the unknown names
  names_standardized2 <- left_join(names_standardized, matches_temp_list[["fuzzy"]][["nomatch"]])
  # Only bind on the minimum required information
  names_standardized2 <- left_join(names_standardized %>% 
                                     select(var_id, variety, intid, new_std_name), data_intid)
  
  
  # Find aliases that may correspond to entries that are already in the database
  matches_temp <- collect_matches(matches_temp_list)
  
  ## Detect aliases
  match_alias <- 
    left_join(names_standardized2, 
              matches_temp, 
              by = "var_id", suffix = c("", ".y")) %>% 
    select(!ends_with(".y")) %>%
    relocate(var_id, variety, variety_db)
  
  alias <- match_alias %>% filter(!is.na(variety_db))
  new_names <- match_alias %>% filter(is.na(variety_db)) %>% select(-c(variety_db, match_step))
  
  # Need the original var_id to rematch once database is read back in
  # "db" means that going into the matching process, variety names and aliases
  # are linked by the variable var_id
  if (match_type == "db"){
  
    new_names2 <- new_names %>% 
      rename(variety_raw = variety) %>%
      mutate(id_newvar = var_id)
    
    new_names_alias <- new_names2 %>% 
      pivot_wider(id_cols = var_id, names_from = type, values_from = new_std_name) %>%
      mutate(id_newvar = var_id) %>%
      ungroup()
    
    supp_info <- new_names2 %>%
      select(-c(type, new_std_name)) %>%
      group_by(id_newvar) %>%
      summarize(across(everything(), ~paste(na.omit(unique(.x)), collapse = "; ")))
    
    new_db_entries <- left_join(new_names_alias, supp_info, by = "id_newvar")
    
  # This is temporary code for match_type == "raw"
  # Likely for more complicated renames, there will have to be a column
  # named parent_id and maybe a new column that accepts "alias", "variety" and "misspelling"
  } else if (match_type == "raw"){
    # names_dbnew_id_newvar1 <- 
    #   new %>% 
    #   rename(variety_raw = variety) %>%
    #   pivot_wider(names_from = type, values_from = new_std_name) %>%
    #   mutate(id_newvar = var_id) %>%
    #   group_by(variety) %>% 
    #   mutate(id_newvar = cur_group_id()) %>% 
    #   ungroup()
    
  }
  
    
  new_db_entries_cb <- add_codebook_cols(new_db_entries, "cultivar", order_cols_by = "codebook_first") %>%
    relocate(variety_raw) %>%
    arrange(crop_type, variety)
  
  if (nrow(alias > 0)){
    message("Writing alias_matches.csv.  Manually add new aliases to entries already present in the db")
    write.csv(alias, paste0(knitroutputfolder, "/","alias_matches.csv"), row.names = FALSE)
  }
  
  
  message("Writing new_db_entries_cb.csv.  Check and edit the entries and then move to auxiliary files")
  write.csv(new_db_entries_cb, paste0(knitroutputfolder, "/","new_db_entries_cb.csv"), row.names = FALSE)
  
  return(list(alias = alias, new = new_names2))
  
}


#' Process standardized new names that were created from process_fuzzymatch()
#' 
#' Once the user checks the new names, they can be read in and processed.
#' The names are checked against the names that still need matches after fuzzymatching
#' (contained in matches_temp_list) and this function:
#' 1. Writes out a csv with a message to update the auxiliary/standardize_new_names*.csv 
#' (New names to standardize arise when the is_truematch status in the fuzzymatching stage
#' moves from 'check' to 'TRUE' or 'FALSE')
#' 2. Writes out a csv with message to add it to the cultivar database
#' 3. Writes out another csv with message to add it to the cv_rename.csv file

#' Note: This function comes after fuzzymatching, just like create_new_db_entries().
#' create_new_db_entries() will not be deprecated because it is used in neely_curation/cultivar
#' but it processes the standardized new names in a way that is unnecessary (finds aliases).
#' Also, this function allows for iterations through the matching process.
#' @param output_nomatch_df The 'nomatch' data.frame created from process_fuzzymatch
#' 
process_std_new_names <- function(output_nomatch_df, auxiliary_files, filename){
  names_standardized <- read.csv(here(auxiliary_files, filename), stringsAsFactors = FALSE) %>%
    mutate(var_id = as.character(var_id))
  
  nomatch_std_name <- left_join(output_nomatch_df %>%
                                  select(-source, n_var_id),
                                names_standardized)  %>%
                      mutate(std_name_status = case_when(is.na(new_std_name) ~ "need new_std_name",
                                       TRUE ~ "add to db"))
  
  add_to_new_std_name <- 
    create_names_nomatch(
      nomatch_std_name %>% filter(std_name_status == "need new_std_name"))
  
  write.csv(add_to_new_std_name, here(knitroutputfolder, "add_to_new_std_name.csv"), row.names = FALSE)
  
  if (nrow(add_to_new_std_name) > 0){
   
    message("Writing add_to_new_std_name.csv. Add these entries to 
            auxiliary_files/standardize_new_names*.csv")
  }
  
  nomatch_std_name_add_to_db <- 
    nomatch_std_name %>%
      filter(std_name_status == "add to db") %>%
      rename(variety_raw = variety) %>%
      rename(variety = new_std_name)

  
  # Return names that won't be matched using exact matching
  # These names will be added to the cv_rename.csv file
  add_to_cv_rename <-  nomatch_std_name_add_to_db %>%
    mutate(intid_db = tolower(gsub("[^A-Za-z0-9+]", "", variety))) %>%
    filter(intid != intid_db) %>% 
    relocate(variety, intid, crop_type) 
  
  write.csv(add_to_cv_rename, paste0(knitroutputfolder, "/","add_to_cv_rename.csv"), row.names = FALSE)
  
  if (nrow(add_to_cv_rename) > 0){
    message("Writing add_to_cv_rename.csv  Add these names to the main cv_rename.csv")
  }
    
  add_to_db1 <-   nomatch_std_name_add_to_db %>%
    group_by(variety) %>%
    summarize(across(everything(), ~paste(na.omit(unique(.x)), collapse = "; ")))
  
  add_to_db <- add_codebook_cols(add_to_db1, "cultivar", order_cols_by = "codebook_first") %>%
    relocate(variety_raw) %>%
    arrange(crop_type, variety)
  
  if (nrow(add_to_db > 0)){
    message("Writing add_to_db.csv.  Add these entries to the database.
            May need to manually fill in extra information such as crop or alias")
    write.csv(add_to_db, paste0(knitroutputfolder, "/","add_to_db.csv"), row.names = FALSE)
  }
  
  return(list(add_to_cv_rename = add_to_cv_rename, add_to_db = add_to_db))
  
}
