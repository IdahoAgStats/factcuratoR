#' Do the steps in the exact match
#'
#' @param data_intid A data.frame created by the function create_intid().
#' If the data.frame has a mix of crop_types, it is best to pass in
#' a column with crop_type for each variety.  Then, the function can ensure that
#' exact matches are for the correct crop_type.  If varieties for only one crop_type
#' are being matched, the select_crops argument can be used
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
#' @inheritParams get_cultivar_rename
#' @inheritParams return.matchgroups
#' @family match variety modules
#' @export
do_exactmatch <- function(db_folder,
                          data_intid,
                          select_before = Sys.Date(),
                          select_crops = NULL,
                          match_type = "raw",
                          rename_df = FALSE,
                          is_blends = FALSE,
                          rename_df_path = NULL){
  if (rename_df) {

    if(is.null(rename_df_path)){stop("Please provide path to the rename file using `rename_df_path`")}

    variety_intid_db <-
      get_cultivar_rename(rename_df_path = rename_df_path) %>%
        select(-c(crop_type))

  } else {

    variety_intid_db <-
      get_variety_db(db_folder = db_folder,
                    select_before = select_before,
                    select_crops = select_crops,
                    for_matching = TRUE)
  }

  exact1 <- left_join(data_intid,
                      variety_intid_db,
                      by = c("intid"),
                      suffix = c("", "_db")) %>%
    # Add intid_db because it is a required column for collect_final_matches()
    mutate(intid_db = intid)

  # Cannot join on "crop_type" = "crop_type_db" above because crop_type may
  # not always be passed in.  If crop_type is NA, then the join will fail.
  # Instead, filter out crop_type mismatches if crop_type is provided
  if ("crop_type" %in% names(exact1)){

    exact1 <- exact1 %>%
                filter(crop_type == crop_type_db | is.na(crop_type == crop_type_db))
  }



  if (match_type == "raw"){
    return_unique = sym("var_id")
  } else if (match_type == "db"){
    return_unique = sym("intid")
  }

  exact2 <- check.anymatch(exact1, checkfor = variety_db, match_type = match_type)
  exact <- return.matchgroups(exact2, is_blends = is_blends)

  message(paste(c("match:", " nomatch:", " check:", " not_needed:"),
                paste(map(exact, ~nrow(.x)), sep = ",")))


  return(exact)

}


#' Get cultivar rename list to match.  The cv_rename.csv is located
#' in the controlled vocabularies.
#' @param rename_df_path The path of the file that contains the variety misspellings
#' @keywords internal
get_cultivar_rename <- function(rename_df_path){

  cv_rename <- read_csv(rename_df_path) %>%
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
    mutate(crop_type_db = crop_type) %>%
    mutate(type_db = NA_character_)

  return(cv_rename2)
}
