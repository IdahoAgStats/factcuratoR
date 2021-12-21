#' Create rules from the codebook using validate::validator()
#'
#' This functions reads in codebooks_all_db.csv and generates rules from the
#' information provided in this main codebook
#'
#' Note: grepl returns FALSE if the tested object is NA,
#' so NAs are not correctly detected in grepl checks.
#' Currently, the validator package does not accept str_detect.
#' As a workaround, for date fields, an extra check for NAs is implemented.
#'
#' @param df_type A string "trial_data" or "trials_metadata" denoting the type of validation
#' rules to generate
#' @inheritParams readin.db
#' @importFrom validate validator
#' @export
#' @family validation functions
create.rules <- function(df_type, db_folder){
  db <- readin.db(db_folder)

  rule_raw <- list.db_var(db_folder, df_type, required_only = FALSE)

  # For trial data, need to pull the codebooks for trial_data and traits
  if (df_type == "trial_data"){
    traits_cb <- db$traits.csv
    rule_raw <- bind_rows(rule_raw, traits_cb %>% rename(variable = trait_name))
  }

  rule_raw <- rule_raw %>%
    select(variable, value_type, values_defined_in, value_range)


  # Format data.frame to write rules
  rule_df1 <- rule_raw %>%
    separate(value_range, into = c("range", "value"), sep = "\\|", remove = FALSE, fill = "right") %>%
    mutate(value = ifelse(str_detect(range, ";"), range, value)) %>%
    mutate(range = ifelse(str_detect(range, "to"), range, NA)) %>%
    separate(range, sep = "to", into = c("min", "max")) %>%
    mutate(max = ifelse(max == "", format(Sys.Date(), "%Y"), max)) %>%
    mutate(value = str_replace_all(value, ";", ",")) %>%
    mutate(is_type = case_when(value_type == "string" ~ "is.character",
                               value_type == "numeric" ~ "is.numeric",
                               value_type == "continuous" ~ "is.numeric",
                               #value_type == "date" ~ "is.Date",
                               # Integer values are checked below using %%1
                               value_type == "integer" ~ NA_character_))

  # Write rules
  rule_df2 <- rule_df1 %>%
    mutate(rule_cb = ifelse(!is.na(values_defined_in),
                            paste(variable, "%in%",  paste0("db[['", values_defined_in, ".csv']][['", variable,"']]")),
                            NA)) %>%
    mutate(rule_range = ifelse(!is.na(min), paste(variable, ">=", min, "&", variable, "<=", max), NA)) %>%
    mutate(rule_value = ifelse(!is.na(value), paste(variable, "%in%", paste0("c(",value,")")), NA)) %>%
    mutate(rule_is.na = ifelse((is.na(rule_cb) &
                                  is.na(rule_range) &
                                  is.na(rule_value) &
                                  !is.na(is_type)),
                               paste0(is_type, "(", variable, ")"), NA)) %>%
    # grepl returns FALSE for NA, so need to also check for NA to correctly return NAs
    mutate(rule_date = ifelse(value_type == "date",
                              paste0("grepl('[0-9]{4}-|/[0-9]{2}-|/[0-9]{2}', ",
                                     variable,
                                     ")"),
                              NA)) %>%
    filter(!is.na(variable)) %>%
    unite("rule", contains("rule"), na.rm = TRUE, sep = "|")


  rule_df2b <- rule_df2 %>%
    filter(value_type == "date") %>%
    mutate(rule = paste0("!is.na(", variable, ")")) %>%
    bind_rows(rule_df2)

  #ifelse(is.na(v1), NA, grepl("test", v1))

  # Rules that are in ADDITION to the rules specified by the codebook
  rule_df3 <- rule_df2b %>%
    mutate(rule = ifelse(rule == "", NA, rule)) %>%
    mutate(rule_int = ifelse(value_type == "integer", paste(variable,"%% 1 == 0"), NA)) %>%
    unite("rule", contains("rule"), na.rm = TRUE, sep = "&") %>%
    mutate(name = variable)

  rules_v <- validate::validator(.data = rule_df3)


}
