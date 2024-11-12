#' Find entries for the cultivar names that need to be checked
#'
#' Summarises how many times a cultivar name shows up in the raw
#' data per year
#' @param df_names A data.frame that contains the column of names to check
#' @param data_raw A data.frame that contains the raw data (all entries)
#' @param join_by A character string or named character string that is passed
#' to the 'by' parameter in left_join()
#' @family match variety functions
#' @export
find_entries_raw_names <- function(df_names, data_raw, join_by){

  join_by_quo <- sym(join_by)

  names_check <- left_join(df_names, data_raw, by = join_by) %>%
    select(all_of(c("var_id", join_by, year)))

  names_check_summary <- names_check %>%
    group_by(!!join_by_quo, year) %>%
    count(name = "n_peryear") %>%
    ungroup() %>%
    group_by(!!join_by_quo) %>%
    summarise(across(.cols = everything(),
                     ~paste(.x, collapse = ";")))

  write.csv(fuzzymatch_summary,
              here::here(knitroutputfolder, "names_check_summary.csv"),
            row.names = FALSE)
}
