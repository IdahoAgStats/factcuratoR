#' Do the steps in the exact match
#'
#' @param data_intid A data.frame created by the function create_intid
#' @param data_suffix A character suffix to be appended to the columns related to the original data
#' @param match_type A string, either "raw" or "db, " which denotes the type of
#' matching procedure to use.  If matching raw variety names, use "raw."
#' If matching database names, use "db."  The difference is that "raw" groups by
#' the var_id and is only looking for one match per var_id.  The method "db"
#' groups by the intid and is looking to match every entry provided.
#' @inheritParams check.anymatch
#' @inheritParams get_variety_db
#' @param rename_df A logical denoting whether to check the rename (misspelling) file
#' Otherwise, the matches are pulled from the database list
#' * Currently, all the programs renames are kept in one file, but may want to
#' consider putting the renames into different files that are collected together
#' See get_cultivar_rename()
#' @inheritParams return.matchgroups
#' @family match variety modules
#' @export
do_exactmatch <- function(db_folder,
                          data_intid,
                          select_before = "2021-01-01",
                          select_crops = NULL,
                          match_type = "raw",
                          rename_df = FALSE,
                          is_blends = FALSE){
  if (rename_df) {
    variety_intid_db <-
      get_cultivar_rename(db_folder = db_folder) %>%
        select(-c(crop_type, crop_type_db))

  } else {

    variety_intid_db <-
      get_variety_db(db_folder = db_folder,
                    select_before = select_before,
                    select_crops = select_crops,
                    for_matching = TRUE)
  }

  exact1 <- left_join(data_intid,
                      variety_intid_db,
                     # It is possible to join by "crop_type" = "crop_type_db"
                     # to ensure that the crop matches, however,
                     # this is currently onlyan issue for the cultivar
                     # 'Salute' and it is better not to implement because sometimes
                     # the crop_type is NA and then wouldn't join
                      by = c("intid"),
                      suffix = c("", "_db")) %>%
    # Add intid_db because it is a required column for collect_final_matches()
    mutate(intid_db = intid)
  # This is a possible start of how to implement filtering out
  # crop_type mismatch (see above), which needs to take into account not
  # removing the entry.  However, if a duplicate entry is created, it does need to be removed
  #  mutate(across(contains("db"),
  #               ~ ifelse(crop_type != crop_type_db, NA,.x)))

  if (match_type == "raw"){
    return_unique = sym("var_id")
  } else if (match_type == "db"){
    return_unique = sym("intid")
  }

  exact2 <- check.anymatch(exact1, checkfor = variety_db, match_type = match_type)
  exact <- return.matchgroups(exact2, is_blends = is_blends)

# This check for >1 match is also in return.matchgroups()
# Unsure if the following code is needed, so commented out for now
#  test_exact <- check.matches(exact[[1]])
#  if (nrow(test_exact) > 0) {
  #   warning("There is more that one database match,
  #        please resolve in the controlled vocabularies")}

  message(paste(c("match:", " nomatch:", " check:", " not_needed:"),
                paste(map(exact, ~nrow(.x)), sep = ",")))


  return(exact)




}


#' Get cultivar rename list to match.  The cv_rename.csv is located
#' in the controlled vocabularies.
#' @inheritParams readin_db
#' @keywords internal
get_cultivar_rename <- function(db_folder){
  db <- readin_db(db_folder = db_folder)

  cv_rename <- db$cv_rename.csv %>%
    rename(variety_db = correct_variety_name) %>% select(-program)

  cv_rename2 = cv_rename %>%
    mutate(intid = tolower(gsub("[^A-Za-z0-9+]", "", wrong_name))) %>% # will remove all characters not specified , which is needed to remove the \ backslash in one of the names
    mutate(intid_db = tolower(gsub("[^A-Za-z0-9+]", "", variety_db))) %>%
    group_by(intid, intid_db) %>%
    # If more than one entry for the same intid and intid_db are listed,
    # these entries are collapsed
    # Note: The wrong_name may be slightly different between the two cases,
    # e.g. "wildfire" versus "Wildfire", but this doesn't matter
    # since the functions match on the intid (which in the example is is "wildfire" for both)
    summarize(across(everything(),
                     ~paste(na.omit(unique(.x)), collapse = ";")),
              .groups = "drop") %>%
    mutate(crop_type = ifelse(crop_type == "", NA, crop_type)) %>%
    mutate(crop_type_db = NA) %>%
    mutate(type_db = NA)

  return(cv_rename2)
}
