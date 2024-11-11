#' Wrapper for create_rules() and validate::confront()
#'
#' This functions summarises the results and fixes the
#' inability for confront() to detect NAs with grepl.
#' See create_rules() for more details.
#' @param df A data.frame to be validated
#' @inheritParams create_rules
#' @param blends A logical denoting whether to check for blends in the variety column.
#' The default is FALSE
#' @importFrom validate confront
#' @export
#' @family validation functions
confront_data <- function(df, df_type, db_folder, blends = FALSE,
                          crop_types = NULL) {

  rules <- create_rules(df_type, db_folder, blends = blends,
                        crop_types = crop_types)

  if (blends) {
    df <- df %>% separate_wider_delim(variety, delim = ";",
                          names = c("variety", "variety2_blend", "variety3_blend"),
                          too_few = "align_start")
  }

  # The database must be read in order for validate::confront to have access
  # to the codebook values
  # Pass the db in as a list (See ?validate::confront for more details)
  # Note: passing in the data in an environment didn't work
  validation_output <- validate::confront(df,
                                          rules,
                                          list(db = readin_db(db_folder)))

  summary <- validate::summary(validation_output) %>% arrange(error, fails)

  date_na <- summary %>%
    filter(str_detect(expression, "!is.na(.+date)")) %>%
    mutate(nNA_temp = fails) %>% select(name, nNA_temp)

  # Fix detection of NAs in data columns
  summary2 <- summary %>%
    # remove suffix from variables that were indexed due to multiple tests
    mutate(name = str_remove_all(name, "\\.\\d$")) %>%
    left_join(date_na, by = "name", multiple = "all") %>%
    mutate(fails = ifelse(str_detect(expression, "grepl"),
                          fails - nNA_temp, fails)) %>%
    mutate(nNA = ifelse(str_detect(expression, "grepl"),
                        nNA + nNA_temp, nNA)) %>%
    select(-nNA_temp) %>%
    filter(!str_detect(expression, "!is.na(.+date)")) %>%
    # remove rows that are duplicates after removing suffixes in the first step
    # this is needed when df_type == "trial_data"
    unique()

  # Add if column is required
  is_req <- list_db_var(db_folder, df_type, required_only = FALSE,
                        crop_types = NULL) %>%
    select(name = variable, required)

  if (df_type == "trial_data") {
    traits_cb <- readin_db(db_folder)$traits %>%
                    select(name = trait_name, crop_type) %>%
                    mutate(required = FALSE) %>%
      filter(crop_type %in% crop_types) # select crops

    is_req <- bind_rows(is_req, traits_cb) %>%
      filter(!is.na(required))
  }

  summary2_req <- left_join(summary2, is_req, by = "name", multiple = "all") %>%
    relocate(required) %>%
    arrange(desc(required))

  return_validate_message(summary2_req)

  return(list(summary = summary2_req, validation_output = validation_output))

}

#' Return a message regarding the status of the validation
#'
#' @param confront_summary A list object that is returned by validate::confront()
#' @keywords internal
return_validate_message <- function(confront_summary){

  temp2 <- confront_summary %>%
    group_by(required) %>%
    select(required, matches(c("fails", "nNA", "error", "warning"))) %>%
    mutate(across(.cols = where(is.numeric), function(x) {
      ifelse(x > 0, TRUE, FALSE) }
      )) %>%
    summarise(across(everything(), .fns = sum))

  if (any(temp2[1,] > 0)) {
    message("Warning: Some issues left to resolve \n",
            paste(capture.output(print(temp2)), collapse = "\n"))
  } else {
    message("Everything looks good")
  }

}

