
#' Validate column names
#'
#' @param df A data.frame with column names to verify against the codebook
#' @param codebook A string denoting the name of the codebook to check the column
#' names against e.g. "trial_data", "trials_metadata"
#' @inheritParams readin.db
#' @export
#' @family validation functions
validate_colnames <- function(df, codebook_name, db_folder){
  db <- readin.db(db_folder)

  cb <- list.db_var(db_folder, codebook_name, required_only = FALSE) %>%
    rename(colname = variable)

  # For trial data, need to pull the codebooks for trial_data and traits
  if (codebook_name == "trial_data"){
    traits_cb <- db$traits.csv %>% mutate(required = FALSE)
    cb <- bind_rows(cb, traits_cb %>% rename(colname = trait_name))
  }

  # Can add the functionality to filter by whether the column name is required
  # according to the codebook
  cb2 <- cb %>%
    filter(!is.na(colname))

  colnames_df <- data.frame(colname = names(df))

  column_report1 <- full_join(colnames_df, cb2,
                              by = "colname",
                              keep = TRUE,
                              suffix = c("_data", "_codebook"))

  column_report2 <-
    column_report1 %>%
    mutate(comment = ifelse(is.na(colname_data), "not present in data", NA)) %>%
    mutate(comment = ifelse(is.na(colname_codebook), paste("not present in codebook:", codebook_name), comment)) %>%
    mutate(comment = ifelse(is.na(comment), "exists in both data and codebook", comment)) %>%
    arrange(comment, required)

  # Output a message to the user
  report2_message <- column_report2 %>% group_by(comment) %>% summarise(n= n())
  missing_required <- column_report2 %>%
    filter(comment == "not present in data") %>%
    group_by(comment, required) %>%
    summarise(req = n()) %>%
    filter(required == TRUE)

  report2_message2 <- left_join(report2_message, missing_required)

  message("Status: \n",
          paste(capture.output(print(report2_message2)), collapse = "\n"))


  return(column_report2)


}
