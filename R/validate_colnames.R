#' Validate column names against codebook
#'
#' @param df A data.frame with column names to verify against the codebook
#' @param codebook A string denoting the name of the codebook to check the column
#' names against e.g. "trial_data", "trials_metadata"
#' @inheritParams readin_db
#' @inheritParams list_db_var
#' @param rm_other_cb A logical denoting whether to remove the other codebook
#' For example, if testing "trial_data", remove the names in df that correspond to "trials_metadata"
#' @export
#' @family validation functions
validate_colnames <- function(df, codebook_name, db_folder, crop_types = NULL,
                              rm_other_cb = NULL){
  db <- readin_db(db_folder)

  cb <- list_db_var(db_folder, codebook_name, required_only = FALSE, crop_types = crop_types) %>%
    rename(colname = variable)


  # Can add the functionality to filter by whether the column name is required
  # according to the codebook, but this is not implemented
  cb2 <- cb %>%
    filter(!is.na(colname)) %>%
    select(colname, required, col_num)

  # Get the names in the data
  colnames_df <- data.frame(colname = names(df))

  column_report1 <- full_join(colnames_df, cb2,
                              by = "colname",
                              keep = TRUE,
                              suffix = c("_data", "_codebook"))

  column_report2 <-
    column_report1 %>%
    mutate(comment =
             case_when(is.na(colname_data) ~ "not present in data",
                       is.na(colname_codebook) ~
                            paste("not present in codebook:", codebook_name),
                       TRUE ~ "exists in both data and codebook")) %>%
    arrange(desc(comment), desc(required))

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

