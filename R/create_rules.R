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
#' @inheritParams readin_db
#' @inheritParams list_db_var
#' @param blends A logical denoting whether there are blends stored in the
#' variety column in df_type = "trial_data"
#' @importFrom validate validator
#' @importFrom tibble add_row
#' @family validation functions
#' @export
create_rules <- function(df_type, db_folder, blends = FALSE, crop_types){
  db <- readin_db(db_folder)
  #Note: create_rules() currently cannot handle if there are duplicate names in the traits file

  rule_raw <- list_db_var(db_folder, df_type, required_only = FALSE, crop_types = crop_types)

  rule_raw <- rule_raw %>%
    select(variable, value_type, values_defined_in, value_range, max_characters)


  # Format data.frame to write rules
  rule_df1 <- rule_raw %>%
    separate_wider_delim(value_range, delim = "|", names = c("range", "value"),
                         too_few = "align_start", cols_remove = FALSE) %>%
    mutate(value = ifelse(str_detect(range, ";"), range, value)) %>%
    mutate(range = ifelse(str_detect(range, "to"), range, NA)) %>%
    separate_wider_delim(range, delim = "to", names = c("min", "max"),
                         too_few = "align_start") %>%
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
    mutate(rule_maxlength =
      ifelse(!is.na(max_characters),
             paste("nchar(as.character(", variable, ")) <=", max_characters),
            NA)) %>%
    # If the rule is na, then check the type
    mutate(rule_is.na = ifelse((is.na(rule_cb) &
                                  is.na(rule_range) &
                                  is.na(rule_value) &
                                  is.na(rule_maxlength) &
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

  # See above.  This code is to ensure grepl returns NA if date is NA
  rule_df2b <- rule_df2 %>%
    filter(value_type == "date") %>%
    mutate(rule = paste0("!is.na(", variable, ")")) %>%
    bind_rows(rule_df2)

  # Rules that are in ADDITION to the rules specified by the codebook
  rule_df3 <- rule_df2b %>%
    mutate(rule = ifelse(rule == "", NA, rule)) %>%
    mutate(rule_int = ifelse(value_type == "integer", paste(variable,"%% 1 == 0"), NA)) %>%
    unite("rule", contains("rule"), na.rm = TRUE, sep = "&") %>%
    mutate(name = variable)

  if (df_type == "trial_data" & blends){
    rule_variety_blend <- rule_df3[which(rule_df3$variable == "variety"),]
    rule_variety_blend$variable <- "variety2_blend"
    rule_variety_blend$name <- "variety2_blend"
    rule_variety_blend$rule <- "variety2_blend %in% db[['cultivar.csv']][['variety']]"

    rule_variety_blend <- rule_variety_blend %>%
      add_row(variable = "variety3_blend", name = "variety3_blend",
                     rule = "variety3_blend %in% db[['cultivar.csv']][['variety']]") %>%
      tidyr::fill(starts_with("value"), .direction = "down")

    rule_df3 <- bind_rows(rule_df3, rule_variety_blend)

  }


  # For blends, check another column called variety2_blend
  if (blends){
    rule_variety_blend <- rule_raw[which(rule_raw$variable == "variety"),]
    rule_variety_blend$variable <- "variety2_blend"

    rule_variety_blend <- rule_variety_blend %>%
      add_row(variable = "variety3_blend") %>%
      tidyr::fill(everything(), .direction = "down")

    rule_raw <- bind_rows(rule_raw, rule_variety_blend)
  }


  rules_v <- validate::validator(.data = rule_df3)


}
