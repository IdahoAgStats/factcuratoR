#' Validate column names against codebook
#'
#' @param df A data.frame with column names to verify against the codebook
#' names against e.g. "trial_data", "trials_metadata"
#' @inheritParams readin_db
#' @inheritParams list_db_var
#' @param cb_name_remove A string denoting the name of the codebook to remove from the data (df)
#' For example, if testing "trial_data", remove the names in df that correspond to "trials_metadata"
#' @export
#' @family validation functions
validate_colnames <- function(df, codebook_name, db_folder, crop_types = NULL,
                              cb_name_remove = NULL){
  db <- readin_db(db_folder)

  cb <- list_db_var(db_folder, codebook_name, required_only = FALSE, crop_types = crop_types) %>%
    rename(colname = variable)

  # Can add the functionality to filter by whether the column name is required
  # according to the codebook, but this is not implemented
  cb2 <- cb %>%
    filter(!is.na(colname)) %>%
    select(colname, required, col_num) %>% unique()

  # Get the names in the data
  # First, filter out the unwanted names:
  if (!is.null(cb_name_remove)){
    cb_remove <-
      list_db_var(db_folder,
                  cb_name_remove,
                  required_only = FALSE,
                  crop_types = NULL) %>%
      rename(colname = variable) %>%
      filter(!colname %in% cb$colname)

    df <- df %>% select(-any_of(cb_remove$colname))
  }
  colnames_df <- data.frame(colname = names(df))

  # special handling for fdk_rep_xx and inf_spikelet_rep_xx where xx should be replaced
  # with 2-digit rep number; fdk should validate between 1 and 5, inf_spikelet 1:30
  if (any(grepl("^fdk_rep_\\d+$|^inf_spikelet_rep_\\d+$", colnames_df$colname))) {

    name_repl <- colnames_df %>% mutate(variable = colname) %>%
      filter(str_detect(colname, "fdk_rep|inf_spikelet_rep")) %>%
      separate_wider_regex(variable,
                           patterns = c(name = ".+", "", index = "\\d{2,}"),
                           too_few = "align_start")

    cb2 <- cb2 %>%
      mutate(colname = str_remove(colname, "xx$")) %>%
      left_join(name_repl, by = c("colname" = "name"), multiple = "all") %>%
      mutate(colname = ifelse(str_detect(colname, "rep_$"), paste0(colname, "xx"), colname),
             index = as.integer(index),
             colname = case_when(
               (str_detect(colname.y, "fdk_rep") & index >= 1 & index <= 5) |
                 (str_detect(colname.y, "inf_spikelet_rep") & index >= 1 & index <= 30) ~ colname.y,
               TRUE ~ colname)) %>%
      select(colname, required, col_num)
  }


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
                       grepl("_xx$", colname_data) ~ "please replace 'xx' with a valid rep number",
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

