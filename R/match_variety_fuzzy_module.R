#' Find fuzzymatch for variety using stringdist using methods "lv" and "jw"
#'
#' Note: This function returns the fuzzymatches from stringdist.variety()
#' and then places stricter requirements on variety names that end in a
#' number with greater than one digit (e.g. the ending number must be
#' an exact match with a string of numbers in the fuzzymatch).  This requirement
#' is to limit the number of close matches for numeric variety names.
#' Because of this condition, must be careful when generating var_id from the raw names
#' to remove any trailing digits that have no meaning (e.g. +25% refers to seed
#' increase and not to a cultivar name)
#'
#' @param var_noexactmatch A data.frame of the varieties without exact matches -
#' the data.frame should contain a column for variety and internal id (intid)
#' @param data_name A string denoting the name of the dataset
#' @inheritParams stringdist.variety
#' @inheritParams get_variety_db
#' @param knitroutputfolder A folder path to send output
#' @importFrom data.table `:=`
#' @family match variety modules
#' @export
find_fuzzymatch <- function(var_noexactmatch,
                            data_name,
                            intid_col,
                            select_before = Sys.Date(),
                            select_crops = NULL,
                            knitroutputfolder = knitroutputfolder,
                            db_folder){

  if (intid_col != "intid")

    {
    intid_sym <- sym(intid_col)
    var_noexactmatch <- var_noexactmatch %>%
      mutate(intid := !!intid_sym) %>%
      select(-!!intid_sym)
    intid_col = "intid"
  }

  quo_intid_col <- sym(intid_col)
  variety_intid_db <- get_variety_db(db_folder = db_folder,
                                    select_before = select_before,
                                    select_crops = select_crops,
                                    for_matching = TRUE)

  # Test that crop_type matches crop_type_db or these will be unnecessarily filtered out
  crop_type_db <- variety_intid_db %>%
    select(crop_type_db) %>%
    unique() # removed dot

  test <- var_noexactmatch %>%
    mutate(crop_match =
             ifelse(crop_type %in% c(crop_type_db$crop_type_db, NA),
                    TRUE, FALSE))

  if (!all(test$crop_match)) {
    warning("Warning: crop_type does not match crop_type_db")
  }
  #

  match_jw <-
    stringdist.variety(var_noexactmatch,
                       variety_intid_db,
                       intid_col = intid_col,
                       best_n = 1,
                       method_stringdist = "jw")


  match_lv <-
    stringdist.variety(var_noexactmatch,
                       variety_intid_db,
                       intid_col = intid_col,
                       best_n = 1,
                       method_stringdist = "lv")

  # The LCS fuzzy matching method was added because Legion and Agripro Legion
  # were not the best matches under "jw" or "lv" methods
  # Filter by "lcs dist_scaled and dist_scaled < 1 as this is all that is necessary
  # to match these names, so hopefully will be sufficient to find similar types of matches
  match_lcs <-
    stringdist.variety(var_noexactmatch,
                       variety_intid_db,
                       intid_col = intid_col,
                       best_n = 1,
                       method_stringdist = "lcs") %>%
    filter(method == "lcs dist_scaled" & dist_scaled < 1)

  match <- bind_rows(match_jw, match_lv, match_lcs) %>%
    relocate(method, dist, dist_scaled, var_id, !!quo_intid_col, intid_db) %>%
    arrange(var_id, !!quo_intid_col, dist, dist_scaled)

  match_filt1 <-
    match %>%
    group_by(var_id, variety_db) %>%
    mutate(method = paste(unique(method), collapse = ";")) %>%
    select(-c(dist, dist_scaled)) %>%
    unique() %>%
    ungroup()

  match_filt2 <-
    match_filt1 %>%
    filter((crop_type == crop_type_db) %>% replace_na(TRUE))

  traildigits_quo <- sym(paste(intid_col, "traildigits", sep = "_"))

  # Test that the final numbers exactly match the fuzzymatch, otherwise
  # too many numeric fuzzymatches will be returned
  match_filt3 <- match_filt2 %>%
    mutate(extract_trail_digits(., !!quo_intid_col, TRUE)) %>%
    mutate(extract_trail_digits(., intid_db, TRUE)) %>%
    mutate(is_string_overlap(., !!traildigits_quo, intid_db_traildigits)) %>%
    filter(is_string_overlap %in% c(TRUE, NA)) %>%
    select(-c(is_string_overlap, !!traildigits_quo, intid_db_traildigits)) %>%
    mutate(is_truematch = "")


  # if (any(str_detect(names(fuzzymatch_all1), "var_id"))){
  #   fuzzymatch_all <- fuzzymatch_all1 %>% arrange(var_id, !!quo_intid_col)
  # } else {
  #   fuzzymatch_all <- fuzzymatch_all1 %>% arrange(!!quo_intid_col)
  # }

  # Add varieties that don't have a fuzzymatch to the end of the data.frame
  # with the status filled in
  # Note: setNames requires entries to be in the reversed order from anti_join
  no_match <- anti_join(var_noexactmatch, match_filt3,
                        by = c(intid_col, "var_id", "variety")) %>%
    mutate(is_truematch = "FALSE")

  matches <- bind_rows(match_filt3, no_match) %>%
    relocate(is_truematch,intid, intid_db,
             variety, variety_db, var_id, db_id, type, type_db,
             crop_db, crop_type, crop_type_db, date_added_db
    )

  message("Writing out fuzzymatch.csv")
  write.csv(matches, paste0(knitroutputfolder, "/","fuzzymatch.csv"), row.names = FALSE, fileEncoding = 'UTF-8')

  return(matches)

}


#' Extract the trailing digits from a column of type character and
#' return the digits in a new column (digits are returned as characters)
#' @param df A data.frame
#' @param x The bare variable name of the column to extract digits
#' @importFrom data.table `:=`
#' @keywords internal
extract_trail_digits <- function(df, x, rm_single){

  x_quo <- enquo(x)
  traildigits_quo <- sym(paste(quo_name(x_quo), "traildigits", sep = "_"))

  df_endnum <- df %>%
    mutate(!!traildigits_quo :=
             stringi::stri_extract_last_regex(!!x_quo, "\\d+")) %>%
    mutate(zeros = str_detect(!!traildigits_quo, "^0+")) %>%
    mutate(!!traildigits_quo :=
             ifelse(zeros, str_remove(!!traildigits_quo, "^0+"), !!traildigits_quo)) %>%
    mutate(!!traildigits_quo :=
             ifelse(!!traildigits_quo == "", NA_character_, !!traildigits_quo)) %>%
    select(!!traildigits_quo) %>%
    mutate(!!traildigits_quo :=
             ifelse(nchar(!!traildigits_quo) == 1 & rm_single == TRUE,
                    NA_character_, !!traildigits_quo)) %>%
    # The NAs must be of character type for is_string_overlap()
    mutate(!!traildigits_quo := ifelse(is.na(!!traildigits_quo), NA_character_, !!traildigits_quo))

  return(df_endnum)
}


#' Check for overlap in strings
#' This function takes a dataframe and compares two columns based on whether
#' the strings overlap.  TRUE is returned if there is an overlap in either direction
#' (it doesn't matter which string is longer)
#' @param df A data.frame
#' @param x1 The bare variable name of the column to compare
#' @param x2 The bare variable name of the column to compare
#' @keywords internal
is_string_overlap <- function(df, x1, x2){

  df_test <- df %>%
    mutate(test1 = stringi::stri_detect_fixed({{x1}}, {{x2}})) %>%
    mutate(test2 = stringi::stri_detect_fixed({{x2}}, {{x1}})) %>%
    mutate(is_string_overlap = ifelse(nchar({{x1}}) > nchar({{x2}}), test1, test2)) %>%
    select(is_string_overlap)

  return(df_test)
}

