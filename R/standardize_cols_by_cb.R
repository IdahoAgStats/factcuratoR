#' Standardize the columns in the order specified by the codebook
#'
#' Contains options to add missing columns or remove unnecessary columns
#' The ordering follows the column order specified in the codebook
#' Any extra columns will come after, in alphabetical order.
#'
#' @inheritParams validate_colnames
#' @param add_missing_cols A logical denoting whether to add missing columns
#' @param required_only A logical denoting whether to add only required columns.
#' This parameter is only called if add_missing_cols = TRUE.
#' Note: This option refers only to missing columns
#' If optional columns exist in the df, these columns are retained.
#' @param codebook_cols_only A logical denoting whether to remove any columns
#' not specified by the codebook
#' @param new_col_fill A value that will be used to fill newly created columns
#' @family standardize to codebook functions
#' @export
standardize_cols_by_cb <- function(df,
                                   codebook_name,
                                   add_missing_cols,
                                   required_only,
                                   codebook_cols_only,
                                   db_folder,
                                   new_col_fill = ""){


  col_summary <- validate_colnames(df, codebook_name, db_folder) %>%
    select(comment, colname_data, colname_codebook, required, col_num) %>%
    mutate(., colname = coalesce(colname_data, colname_codebook))

  # Add columns
  if (add_missing_cols){

    col_add <- col_summary %>% filter(comment == "not present in data")

    if (required_only){ col_add <- col_add %>% filter(required == TRUE) }
    col_add_names <- col_add$colname

    message("Adding columns: ", paste(col_add_names, collapse = ", "))
    df[, col_add_names] <- new_col_fill
    df_names <- names(df)
  }

  # If codebook_cols_only, remove columns from the dataset
  # that are NOT included in the codebook
  if (codebook_cols_only){
    cols_rm <- (col_summary %>%
                  filter(
                    str_detect(comment, "not present in codebook:")))$colname

    if (length(cols_rm)>0){
      message("Removing cols not in codebook: ", paste(cols_rm, collapse = ", "))
      col_summary <- col_summary %>%
        filter(!str_detect(comment, "not present in codebook:"))
    }

  }

  # Relocate columns in a dataset based on their order in the codebook
  col_summary_ordered <- col_summary %>% arrange(col_num, colname)
  df <- df %>% select(any_of(col_summary_ordered$colname))

  return(df)

}



