#' Wrapper for confront() from the validator package
#'
#' This functions summarises the results and fixes the
#' inability for confront() to detect NAs with grepl.
#' See create.rules() for more details.
#' @param df A data.frame to be validated
#' @inheritParams create.rules
#' @importFrom validate confront
#' @export
#' @family validation functions
confront_data <- function(df, df_type, db_folder){

  # The database must be read in order for validate::confront to have access
  # to the codebook values
  # The database must be saved to a new environment that is passed to
  # validate::confront (See ?validate::confront for more details)
  validation_env <- new.env()
  validation_env$db <- readin.db(db_folder)

  rules <- create.rules(df_type, db_folder)

  validation_output <- validate::confront(df, rules, validation_env)

  summary <- validate::summary(validation_output) %>% arrange(error, fails)

  date_na <- summary %>%
    filter(str_detect(expression, "!is.na(.+date)")) %>%
    mutate(nNA_temp = fails) %>% select(name, nNA_temp)

  # Fix detection of NAs in data columns
  summary2 <- left_join(summary, date_na) %>%
    mutate(fails = ifelse(str_detect(expression, "grepl"), fails - nNA_temp, fails)) %>%
    mutate(nNA = ifelse(str_detect(expression, "grepl"), nNA + nNA_temp, nNA)) %>%
    select(-nNA_temp) %>%
    filter(!str_detect(expression, "!is.na(.+date)"))

  return_validate_message(summary2)

  return(list(summary = summary2, validation_output = validation_output))
}

#' Return a message regarding the status of the validation
#'
#' @param output_confront An object that is returned by validate::confront()
#' @keyword internal
return_validate_message <- function(confront_summary){

  temp2 <- confront_summary %>%
    select(matches(c("fails", "nNA", "error", "warning"))) %>%
    mutate(across(.cols = where(is.numeric), function(x){ ifelse(x > 0, TRUE, FALSE)})) %>%
    summarise(across(, .fns = sum))

  if (any(temp2[1,] > 0)){
    message("Warning: Some issues left to resolve \n",
            paste(capture.output(print(temp2)), collapse = "\n"))
  } else {
    message("Everything looks good")
  }

}
