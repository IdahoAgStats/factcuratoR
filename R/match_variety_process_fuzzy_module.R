#' Module to process fuzzymatches given an auxiliary file
#'
#' This function writes out:
#' - fuzzy_check.csv (A file of fuzzy matches to manually check)
#' !Note: Rows for which the source column is only "new_fuzzymatch" must be
#'  manually pasted into the aux_fuzzy_status file
#'  Source column containing "csv" denotes that the possible fuzzymatch in
#'  the aux_fuzzy_status file is blank (or not TRUE or FALSE)
#'
#' - add_fuzzy_to_cv_rename.csv (A file to paste into the main
#' controlled_vocab/cv_rename.csv)
#' These are names for which is_truematch = TRUE and these names
#' should be manually added to controlled_vocab/cv_rename.csv file so they are
#' caught using do_exactmatch()
#'
#' - standardize_new_names.csv (A file of names to standardize for
#' adding to the database.)
#'
#' @inheritParams bind_fuzzymatches
#' @param knitroutputfolder A path to send output
#' @family match variety modules
#' @export
process_fuzzymatch <- function(output_fuzzymatch_df,
                               aux_fuzzy_status,
                               knitroutputfolder){

  is_same_var_id <- assure_var_id(output_fuzzymatch_df, aux_fuzzy_status)
  if (is_same_var_id == FALSE){
    warning("var_id has changed between the two inputs. Setting aux_fuzzy_status var_id to NA")
    aux_fuzzy_status <- aux_fuzzy_status %>% mutate(var_id = NA_character_)
  }

  # Bind the fuzzy_status with the newly generated fuzzymatches
  # e.g. if there have been updates to the database codebook or to the
  # fuzzymatching functions, then new matches may arise in the fuzzymatching stage
  # This step means that in this case, the new fuzzy_status file does not need
  # to be filled out from scratch
  # Warning: if intid numbers (var_id?) change, this will cause problems
  # However, cultivar names may be removed after creating intid after the fuzzy_status file is filled in
  # Note: Can modify to allow is.na(var_id) to pass, if want to be able to add new cultivars at this stage of the matching
  fuzzy_status <-
    bind_rows(output_fuzzymatch_df %>%
                mutate(is_truematch = as.logical(is_truematch)) %>%
                mutate(source = "new_fuzzymatch") %>%
                mutate(db_id = as.character(db_id)) %>%
                mutate(var_id = as.character(var_id)),
              aux_fuzzy_status %>%
                mutate(is_truematch = as.logical(is_truematch)) %>%
                mutate(source = "csv") %>%
                mutate(db_id = as.character(db_id)) %>%
                mutate(var_id = as.character(var_id))) %>%
    mutate(across(where(is.character), ~ifelse(.=="", NA, .))) %>%
    #group_by(var_id, intid, variety, intid_db) %>%
    group_by(intid, variety, intid_db) %>%
    summarize(across(everything(), ~paste(na.omit(unique(.x)), collapse = "; "))) %>%
    ungroup() %>%
    # Filter the data.frame to only accept varieties that are in the newly generated list of fuzzy_matches
    # Some reasons to remove varieties that are not currently in the newly created
    # fuzzy_matches list:
    # - if the fuzzymatching algorithm has been updated/ improved and so less bad matches are returned
    # - in the new process, where a list of misspellings are kept, this removes cultivars from having
    # - to be checked again
    filter(source != "csv")


  results_fuzzymatch <-
    clean_manualfuzzy(match_type = "raw", df = fuzzy_status) # Unsure if match_type = "raw" matters


    fuzzy_check <- results_fuzzymatch[["check"]] %>%
      relocate(is_truematch,intid, intid_db,
               variety, variety_db, var_id, db_id, type, type_db,
               crop_db, crop_type, crop_type_db, date_added_db) %>%
      arrange(source)


    write.csv(fuzzy_check,
              paste(knitroutputfolder, "fuzzy_check.csv", sep = "/"), row.names = FALSE)

    message("Writing out fuzzy_check.csv.  Check these fuzzymatches.
            Add source = new_fuzzymatches to the aux_fuzzy_status file.")


  ## Create add_fuzzy_to_cv_rename.csv
  # Need to write out intid as the "wrong name" in order to match the cleaned up intid
  add_fuzzy_to_cv_rename <- results_fuzzymatch[["match"]] %>%
    relocate(variety_db, intid, crop_type)
  write.csv(add_fuzzy_to_cv_rename, paste0(knitroutputfolder, "/","add_fuzzy_to_cv_rename.csv"), row.names = FALSE)
  message("Writing add_fuzzy_to_cv_rename.csv  Add these names to the main cv_rename.csv")


  # Remove any duplicates caused by adding new rows to the
  # "output_fuzzymatch_status.csv" that may have a different (or updated crop type)
  # Specifically, this was an issue because the cultivar "Fredro" had an unknown type
  # Only process the results_fuzzymatch[["nomatch"]] data.frame if there are entries
  # (This ifelse statement was added because the mutate(across) was causing type problems
  # if there weren't any entries.  Specifically var_id and other columns were
  # being converted to logical when they needed to be character.
  if (nrow(results_fuzzymatch[["nomatch"]]) > 0){
    no_match1 <- results_fuzzymatch[["nomatch"]] %>%
      mutate(across(where(is.character), ~ifelse(.=="", NA_character_, .))) %>%
      group_by(var_id, variety, intid) %>%
      summarize(across(everything(), ~paste(na.omit(unique(.x)), collapse = "; "))) %>%
      ungroup() %>%
      add_count(var_id, name = "n_var_id") %>%
      filter(n_var_id < 2 | (n_var_id ==2 & type == "variety"))
  } else {
    no_match1 <- results_fuzzymatch[["nomatch"]] %>% mutate(n_var_id = NA_integer_)
  }

  results_fuzzymatch[["nomatch"]] <- no_match1

  names_nomatch <- create_names_nomatch(no_match1)

  write.csv(names_nomatch,
            paste0(knitroutputfolder, "/","standardize_new_names.csv"),
            row.names = FALSE)

  message("Writing standardize_new_names.csv.
          If a cultivar has more than one spelling or formatting,
          standardize the name in the new_std_name column")


  message(paste(c("match:", " nomatch:", " check:", " not_needed:"),
                paste(map(results_fuzzymatch, ~nrow(.x)), sep = ",")))

  return(results_fuzzymatch)

}

#' Assure that var_id is the same for the current outputfuzzymatch_df
#' and the aux_fuzzy_status that is read in
#' @inheritParams bind_fuzzymatches
#' @keywords internal
assure_var_id <- function(output_fuzzymatch_df, aux_fuzzy_status){
  fuzzymatch_output_names <- output_fuzzymatch_df %>% select(intid, var_id)
  aux_names <- aux_fuzzy_status %>% select(intid, var_id)

  var_id_check <- anti_join(
    fuzzymatch_output_names,
    aux_names, by = c("intid", "var_id"))

  if (nrow(var_id_check)>0){
    is_same_var_id = FALSE
  } else {
    is_same_var_id = TRUE
  }

    return(is_same_var_id)

}

######
#' Module to process fuzzymatches given an auxiliary file
#'
#' @inheritParams bind_fuzzymatches
deprecated_process_fuzzymatch <- function(output_fuzzymatch_df, aux_fuzzy_status){

  output_fuzzymatch_status1 <-
    bind_fuzzymatches(output_fuzzymatch_df = output_fuzzymatch_df,
                      aux_fuzzy_status = aux_fuzzy_status)

  # Filter the data.frame to only accept varieties that are in the newly generated list of matches
  # Cultivar names may be removed after creating intid after the fuzzy_status file is filled in
  # This step means that in this case, the fuzzy_status file does not need to be filled out again
  # Warning: if intid numbers change, this will cause problems
  # Note: Can modify to allow is.na(var_id) to pass, if want to be able to add new cultivars at this stage of the matching
  output_fuzzymatch_status <-
    output_fuzzymatch_status1 %>%
    filter(var_id %in% output_fuzzymatch_df$var_id)

  results_fuzzymatch <-
    clean_manualfuzzy(match_type = "raw", df = output_fuzzymatch_status) # Unsure if match_type = "raw" matters

  #
  new_fuzzy <- output_fuzzymatch_status %>% filter(is.na(is_truematch))
  if (nrow(new_fuzzy)>0){
    write.csv(new_fuzzy, paste(knitroutputfolder, "new_fuzzy.csv", sep = "/"), row.names = FALSE)
    message("New fuzzymatches have been generated")
  }

  # Remove any duplicates caused by adding new rows to the
  # "output_fuzzymatch_status.csv" that may have a different (or updated crop type)
  # Specfically, this was an issue because the cultivar "Fredro" had an unknown type
  no_match1 <- results_fuzzymatch[["nomatch"]] %>%
    group_by(var_id, variety, intid) %>%
    slice(which.max(!is.na(crop_type)))

  results_fuzzymatch[["nomatch"]] <- no_match1

  names_nomatch <- create_names_nomatch(no_match1)

  write.csv(names_nomatch, paste0(knitroutputfolder, "/","standardize_new_names.csv"), row.names = FALSE)
  message("Writing standardize_new_names.csv.  If a cultivar has more than one spelling or formatting, standardize the name in the new_std_name column")


  message(paste(c("match:", " nomatch:", " check:", " not_needed:"),
                paste(map(results_fuzzymatch, ~nrow(.x)), sep = ",")))

  return(results_fuzzymatch)

}



