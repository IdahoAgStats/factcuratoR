#' Match names with the new db entries (deprecated)
#'
#' As of 2022/01/11, this function is deprecated
#' Note: It is important that the "id_newvar" column refers to the same
#' entries between the output created by create_new_db_entries() and the auxiliary
#' new cultivar controlled vocabulary names.
#'
#' @param filename A filename that contains the information
#'   created by create_new_db_entries() and checked and filled in by the collaborators
#' @param names_newvarid A data.frame of variety names that do not have matches.  This data.frame
#'   is a list element named 'names_newvarid' generated using create_newdbnames()
#' @param dots Bare variables to select from the new cultivar name
#' @keywords internal
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
#' `collect_final_matches()` combines the match output lists and returns a data frame
#' containing the raw names, matched clean names, and the stage where they were matched.
#' An output file, 'final_matches.csv', will be written to the specified output location.
#'
#' Note: If no match has been found, then the returned match is the original raw name.
#'
#' @param match_list A list of the output from each step of the variety matching process,
#' i.e., exact, rename, and fuzzy. These must be enclosed in a `list()` call.
#' @param names_raw A data.frame of the names created by `create_intid()`
#' @param csv_suffix Optional string denoting the suffix for the .csv name.
#' @param knitroutputfolder A path to the output location
#' @param is_blends A logical that specifies whether the varieties are blends.
#' Default is FALSE
#' @family match variety functions
#' @export
collect_final_matches <- function(match_list,
                                  names_raw,
                                  knitroutputfolder,
                                  csv_suffix = NULL,
                                  is_blends = FALSE){

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
    select(var_id, variety, variety_db, match_step, intid, intid_db, type_db) %>%
    ungroup()

  names_raw2 <- names_raw %>%
    group_by(var_id) %>%
    add_count(name = "n_var_id") %>%
    filter(!(n_var_id > 1 & type == "alias")) %>%
    ungroup()

  if (is_blends){
    join_by_var <- c("variety", "var_id", "intid")
  } else {
    join_by_var <- c("variety", "var_id")
  }

  final_matches <- left_join(names_raw2,
                             names_matches2,
                             by = join_by_var,
                             suffix = c("", "_matches"))

  n_unmatched_names <- sum(is.na(final_matches$variety_db))

  if (n_unmatched_names > 0){
    message(paste("Warning: ", n_unmatched_names , "cultivar(s) not yet matched to the database.
                      The raw name is being returned"))

    if (is_blends){
      temp_name <- sym("intid")
    } else{
      temp_name <- sym("variety")
    }
    final_matches <- final_matches %>%
      mutate(variety_db = ifelse(is.na(variety_db), !!temp_name, variety_db))
  }

  # Collapse both matched names
  if (is_blends){
    final_matches <- final_matches %>%
      group_by(var_id, variety) %>%
      summarize(across(everything(), ~paste(na.omit(unique(.x)), collapse = ";")),
                .groups = "drop")
  }

  write.csv(final_matches, paste0(knitroutputfolder, "/","final_matches", csv_suffix, ".csv"), row.names = FALSE)
  message("Writing final_matches.csv")

  return(final_matches)

}
