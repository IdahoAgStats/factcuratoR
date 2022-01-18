#' Wrapper for create_rules() and validate::confront()
#'
#' This functions summarises the results and fixes the
#' inability for confront() to detect NAs with grepl.
#' See create_rules() for more details.
#' @param df A data.frame to be validated
#' @inheritParams create_rules
#' @importFrom validate confront
#' @export
#' @family validation functions
confront_data <- function(df, df_type, db_folder){

  rules <- create_rules(df_type, db_folder)

  # The database must be read in order for validate::confront to have access
  # to the codebook values
  # Pass the db in as a list (See ?validate::confront for more details)
  # Note: passing in the data in an environment didn't work
  validation_output <- validate::confront(df, rules, list(db = readin_db(db_folder)))

  summary <- validate::summary(validation_output) %>% arrange(error, fails)

  date_na <- summary %>%
    filter(str_detect(expression, "!is.na(.+date)")) %>%
    mutate(nNA_temp = fails) %>% select(name, nNA_temp)

  # Fix detection of NAs in data columns
  summary2 <- left_join(summary, date_na, by = "name") %>%
    mutate(fails = ifelse(str_detect(expression, "grepl"), fails - nNA_temp, fails)) %>%
    mutate(nNA = ifelse(str_detect(expression, "grepl"), nNA + nNA_temp, nNA)) %>%
    select(-nNA_temp) %>%
    filter(!str_detect(expression, "!is.na(.+date)"))

  # Add if column is required
  is_req <- list_db_var(db_folder, df_type, required_only = FALSE) %>%
    select(name = variable, required)

  if (df_type == "trial_data"){
    traits_cb <- readin_db(db_folder)$traits %>%
                    select(name = trait_name, crop_type) %>%
                    mutate(required = FALSE)

    is_req <- bind_rows(is_req, traits_cb)
  }

  summary2_req <- left_join(summary2, is_req, by = "name") %>%
    relocate(required) %>%
    arrange(desc(required))

  return_validate_message(summary2_req)

  return(list(summary = summary2_req, validation_output = validation_output))
}

#' Return a message regarding the status of the validation
#'
#' @param output_confront An object that is returned by validate::confront()
#' @keywords internal
return_validate_message <- function(confront_summary){

  temp2 <- confront_summary %>%
    group_by(required) %>%
    select(required, matches(c("fails", "nNA", "error", "warning"))) %>%
    mutate(across(.cols = where(is.numeric), function(x){ ifelse(x > 0, TRUE, FALSE)})) %>%
    summarise(across(everything(), .fns = sum))

  if (any(temp2[1,] > 0)){
    message("Warning: Some issues left to resolve \n",
            paste(capture.output(print(temp2)), collapse = "\n"))
  } else {
    message("Everything looks good")
  }

}

