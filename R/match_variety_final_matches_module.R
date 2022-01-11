#' Match names with the new db entries
#' Note: It is important that the "id_newvar" column refers to the same
#' entries between the output created by create_new_db_entries() and the auxiliary
#' new cultivar controlled vocabulary names.
#'
#' @param filename A filename that contains the information
#'   created by create_new_db_entries() and checked and filled in by the collaborators
#' @param names_newvarid A data.frame of variety names that do not have matches.  This data.frame
#'   is a list element named 'names_newvarid' generated using create_newdbnames()
#' @param dots Bare variables to select from the new cultivar name
match_newdb <- function(auxiliary_files, filename, names_newvarid, ...){

  dots <- enquos(...)
  join_id <- "id_newvar"

  quo_join_id <- enquo(join_id)

  names_newdb_pi <- read.csv(paste(auxiliary_files, filename, sep = "/"),
                             stringsAsFactors = FALSE) %>%
    mutate(across(.cols = starts_with("var_id"), ~as.character(.x))) %>%
    mutate(id_newvar = as.character(id_newvar))

  names_new_match <- left_join(names_newvarid,
                               names_newdb_pi %>%
                                 rename(variety_db = variety) %>%
                                 select(-c(alias, intid, variety_raw)),
                               by = join_id,
                               suffix = c("_original", ""))

  results_new_match <- names_new_match %>%
    rename(variety = variety_raw) %>%
    select(- c(new_std_name, contains("_original")))

  test_all_matched <- results_new_match %>% filter(is.na(variety_db))

  if (nrow(test_all_matched) > 0) {
    warning("Warning: some cultivars don't have a new database entry.
            Please resolve this issue.")
  }

  return(list(match = results_new_match))
}



#' Module to collect all the matches from each stage of the variety matching process
#'
#' Note: If no match has been found, then the returned match == the original name
#'
#' @param match_list A list of the output from each step of the variety matching process
#' @param names_raw A data.frame of the names created by create_intid()
#' @param csv_suffix A string denoting the suffix for the .csv name
#' @param knitroutputfolder A path to the output location
#' @family match variety functions
#' @export
collect_final_matches <- function(match_list, names_raw, knitroutputfolder){

  # Return all columns from each step
  colnames <- match_list %>%
    map(., pluck("match")) %>%
      map(., function(x){ colnames(x)}) %>%
    unlist() %>%
    unique(.)

  # Remove these columns for now because they differ in type (they are not a priority)
  colnames <- colnames[!colnames %in% c("db_id", "date_added_db", "is_truematch")]

  cols_syms <- syms(colnames)

  names_matches1 <- collect_matches(match_list, !!!cols_syms)


  names_matches2 <- names_matches1 %>%
    unique(.) %>%
    group_by(var_id) %>%
    add_count(name = "n_var_id") %>%
    filter(!(n_var_id > 1 & type == "alias")) %>% # only keep the variety match (want one entry per var_id)
    select(var_id, variety, variety_db, match_step, intid, intid_db, type_db)
  names_raw2 <- names_raw %>%
    group_by(var_id) %>%
    add_count(name = "n_var_id") %>%
    filter(!(n_var_id > 1 & type == "alias"))

  final_matches <- left_join(names_raw2,
                             names_matches2,
                             by = c("variety", "var_id"),
                             suffix = c("", "_matches"))

  n_unmatched_names <- sum(is.na(final_matches$variety_db))

  if (n_unmatched_names > 0){
    message(paste("Warning: ", n_unmatched_names , "cultivar(s) not yet matched to the database.
                      The raw name is being returned"))

    final_matches <- final_matches %>%
      mutate(variety_db = ifelse(is.na(variety_db), variety, variety_db))
  }



  write.csv(final_matches, paste0(knitroutputfolder, "/","final_matches", ".csv"), row.names = FALSE)
  message("Writing final_matches.csv")

  return(final_matches)

}
