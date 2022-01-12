#' Find column information
#'
#' For a data.frame, loop through the columns provided (cols_check)
#' and summarise the number of non-NA entries and the groups that
#' contain non-NA entries (for example if want to search trial, year, or filename),
#' and return the first non-NA entry as an example
#'
#' @param df A data.frame to summarise
#' @param cols_check A character vector of column names to check
#' @param by_col A bare variable to summarise
#' @family validation functions
#' @export
find_col_info <- function(df, cols_check, by_col){
  df2 <- df %>%
    mutate(across(.cols = everything(),
                  .fns = ~ ifelse(.x == -9, NA, .x)))

  ans <- map(cols_check, function(x){
    x_sym <- sym(x)
    ans <- df2 %>%
      filter(!is.na(!!x_sym)) %>%
      dplyr::summarise(n = n(),  # count the total number of entries
                       contained_in = paste(unique({{by_col}}), collapse = ";")) %>%
      unique(.) %>%
      mutate(variable = x) %>%
      mutate(example = as.character(first(na.omit(df2[[x]]))))
  })

  bind_rows(ans) %>% arrange(variable)
}
