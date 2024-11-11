#' Fuzzymatch varieties using stringdist_join
#'
#' Fuzzymatch the varieties from the raw data with the database using an internal id.
#' This function uses stringdist_join and returns the closest match (smallest distance)
#' To have enough flexibility without returning too many fuzzy matches,
#' this function returns the closest match measured in 2 different ways:
#' 1. select the best_n varieties with the smallest distance, and then select the
#' best_n varieties that have the smallest scaled distance
#' 2. select the best_n varieties with the smallest scaled distance, irrespective
#' of the raw distance
#' See the effect of these different selection methods in test_match_variety_fns.R
#' @param raw_data A data.frame
#' @param database A data.frame
#' @param suffix A string denoting the name of the raw data
#' @param intid_col A string denoting the name of the internal id column
#'   used to join the raw data and database
#' @param best_n A numeric denoting the number of best matches to select
#' @param method A string corresponding to a string metric defined by stringdist
#'   e.g. "lw", "jw"
#' @importFrom fuzzyjoin stringdist_join
#' @keywords internal
stringdist.variety <- function(raw_data,
                               database,
                               intid_col = "name",
                               best_n,
                               method_stringdist){

  quo_initid_col <- sym(intid_col)

  all_stringdist_matches1 <- stringdist_join(raw_data, database,
                           by = intid_col,
                           mode = "left",
                           ignore_case = FALSE,
                           method = method_stringdist,
                           max_dist = 99,
                           distance_col = "dist")

  all_stringdist_matches2 <- all_stringdist_matches1 %>%
    rename_with(.cols = ends_with(".x"),
                ~ gsub("\\.x", "", .x)) %>%
    rename_with(.cols = ends_with(".y"),
                ~ gsub("\\.y", "_db", .x))

  all_stringdist_matches3 <- all_stringdist_matches2 %>%
    group_by(!!quo_initid_col) %>%
    mutate(method = paste(method_stringdist, "dist")) %>%
    mutate(dist_scaled = ifelse(method == "jw",
                                dist,
                                dist / nchar(intid_db))) %>%
    slice_min(order_by = dist, n = best_n) %>%
    slice_min(order_by = dist_scaled, n = best_n)

  all_stringdist_matches4 <- all_stringdist_matches2 %>%
    group_by(!!quo_initid_col) %>%
    mutate(method = paste(method_stringdist, "dist_scaled")) %>%
    mutate(dist_scaled = dist / nchar(intid_db)) %>%
    slice_min(order_by = dist_scaled, n = best_n)

  all_stringdist_matches5 <- bind_rows(all_stringdist_matches3, all_stringdist_matches4)

  return(all_stringdist_matches5)
}



#' Check for any matches
#'
#' After a variety name match (exact or fuzzymatch), check whether there are any
#' exact or approved matches.
#' @param df A data.frame with variety names matched to database names
#' @param group A column name by which to group and search for a match
#' (e.g. to determine if there are any matches for the variety column, use: variety)
#' @param checkfor A column name to check whether there is a match
#' (This parameter is used only for exact matching.  For fuzzymatches, the status of the match
#' must be manually entered)
#' @param match_type A string that is either 'db' or 'raw', which refers to processing
#' database entries (only one entry per cultivar) or raw entries which may have many
#' alternate spellings for one cultivar
#' @keywords internal
check.anymatch <- function(df, group, checkfor = NULL, match_type){

  quo_checkfor <- enquo(checkfor)

  if (!("is_truematch" %in% colnames(df))) {
    df <- df %>% mutate(is_truematch = ifelse(is.na(!!quo_checkfor),
                                              FALSE, TRUE))
  }

  # Standardize is_truematch to logical
  df <- df %>% mutate(is_truematch =
                        ifelse(!is_truematch %in% c(TRUE, FALSE),
                               NA, is_truematch)) %>%
    mutate(is_truematch = as.logical(is_truematch))

  if (match_type == "raw") {
    quo_group = sym("var_id")
    df_check <- df %>%
      group_by(!!quo_group)
  } else if (match_type == "db") {

    quo_group = syms(c("intid", "var_id"))
    df_check <- df %>%
      group_by(!!!quo_group)
  }


  df_check2 <- df_check %>%
    mutate(any_match = any(is_truematch  == "TRUE")) %>%
    ungroup()
  return(df_check2)
}

#' Return the variety names by whether they have a match, no match, or need to be checked
#'
#' Return the variety names.  This function takes a data.frame of variety names
#' and their potential database matches.  Given the columns status, which denotes
#' whether the variety has a database match, and any_match, which denotes whether
#' the variety has at least one match, this function separates the data.frame into
#' a list of 4 data.frames: the varieties with database matches, the varieties without
#' database matches, the varieties that still need to be checked manually (with a collaborator),
#' and the duplicate rows of varieties that already have matches
#' @param df_any_match A data.frame that has been processed by check.anymatch()
#' @param is_blends A logical that specifies whether the varieties are blends.
#' Default is FALSE
#' @keywords internal
return.matchgroups <- function(df_any_match, is_blends = FALSE){
  df_any_match <- df_any_match %>% mutate(var_id = as.character(var_id))

  # Set the group (match, nomatch, check, or not_needed)
  if (is_blends) {
    df_any_match_groups <-
      df_any_match %>%
      ungroup() %>%
      mutate(group = case_when(
        is_truematch == "TRUE" ~ "match",
        is_truematch == "FALSE"  ~ "nomatch",
        !is_truematch %in% c("TRUE", "FALSE", "alias") ~ "check"
      ))

  } else {
    df_any_match_groups <-
      df_any_match %>%
      ungroup() %>%
      mutate(group = case_when(
        (is_truematch == "TRUE" & any_match == TRUE) ~ "match",
        ((is_truematch == "FALSE" | is_truematch == "alias") &
           any_match == FALSE) ~ "nomatch",
        (!is_truematch %in% c("TRUE", "FALSE", "alias") &
           any_match == FALSE) ~ "check",
        (!is_truematch %in% c("TRUE") & any_match == TRUE) ~ "not_needed"
      )) %>%
      mutate(group = case_when(is.na(group) & is.na(is_truematch) ~ "check",
                             is.na(group) & is_truematch == "FALSE" ~ "not_needed",
           TRUE ~ group))
  }

  # Separate groups into individual data.frames
  match <- df_any_match_groups %>% filter(group == "match")
  nomatch <- df_any_match_groups %>% filter(group == "nomatch") %>%
                 select(!matches("_db$|db_id|is_truematch|any_match|method")) %>%
                 unique(.)
  match_check <- df_any_match_groups %>% filter(group == "check")
  not_needed <- df_any_match_groups %>% filter(group == "not_needed")

  # Add the groups into a list
  match_list <- list(match, nomatch, match_check, not_needed) %>%
    map(., ~.x %>% select(-group))
  names(match_list) <- c("match", "nomatch", "check", "not_needed")

  # Check that only one database entry matches a given var_id
  # This helps to catch duplicate entries in the controlled vocabularies
  # Note: if is_blends = TRUE, this is not checked because one entry is supposed
  # to match multiple db_ids
  if (!is_blends & all(c("db_id", "var_id") %in% names(match_list[["match"]]))) {

    test_multmatch <- check.matches(match_list[["match"]])
    if (nrow(test_multmatch > 0)) {
      warning(paste("Warning: var_id matches with more than one db_id. Please reconcile potential error in database. Use factcuratoR:::check.matches(result[[1]]) to see the varieties that match more than one database name.",
                   paste0(capture.output(test_multmatch), collapse = "\n")))
    }

    # Remove alias if there is a match with variety
    test_aliasdup <- rm.alias_dupmatch(match_list[["match"]])

    if (nrow(test_aliasdup[["not_needed"]] > 0)) {
      match_list[["match"]] <- test_aliasdup[["match"]]

      match_list[["not_needed"]] <- rbind(match_list[["not_needed"]], test_aliasdup[["not_needed"]])
    }

  } else if (!is_blends) {
    message("var_id and db_id do not exist. test_multmatch and test_aliasdup not run")
  }

  return(match_list)
}


#' Check that matches have one database match
#'
#' @param match_df A data.frame with variety names and database matches
#' (including volumns var_id and db_id)
#' @keywords internal
check.matches <- function(match_df){

  mult_matches <- match_df %>%
    select(var_id, db_id) %>%
    unique(.) %>%
    group_by(var_id) %>%
    add_count() %>%
    filter(n > 1)

  mult_entries <- match_df %>%
    filter(var_id %in% mult_matches$var_id)

  return(mult_entries)
}

#' Remove alias if there is a match with variety
#'
#' If a variety is listed in the raw data with an alias,
#' both names may match to names in the database.
#' This function will remove the alias and return the listed variety name
#'
#' @param match_df A data.frame that contains var_id, db_id, and type_db
#' ("alias" or "variety")
#' @keywords internal
rm.alias_dupmatch <- function(match_df) {

  mult_matches <- match_df %>%
    select(var_id, db_id, type_db) %>%
    unique(.) %>%
    group_by(var_id, db_id) %>%
    add_count() %>%
    filter(n > 1) %>%
    filter(type_db == "alias") %>% select(-n) %>% ungroup()

  df_clean <- anti_join(match_df, mult_matches, by = c("var_id", "db_id", "type_db"))
  df_rm_alias <- left_join(mult_matches, match_df, by = c("var_id", "db_id", "type_db"))

  ls <- list(match = df_clean, not_needed = df_rm_alias)
  return(ls)
}

#' Clean the file of fuzzy matches with manual checks
#'
#' Fuzzy matching outputs a file of potential/fuzzy matches.
#' A user must go in a assign whether the fuzzy match is a true match.
# @param curation_folder A path to the curation folder
#' @inheritParams check.anymatch
#' @inheritParams return.matchgroups
#' @param df A data.frame, which can be provided in lieu of the curation_folder
#' and filename
#' @keywords internal
clean_manualfuzzy <- function(match_type = "raw", df){

  if (match_type == "raw") {
    return_unique <- sym("var_id")
  } else if (match_type == "db") {
    return_unique <- sym("intid")
  }

  # Unsure why previously used eval(substitute()), however, this code doesn't work for process_fuzzymatch()
  #fuzzy_check2 <- eval(substitute(check.anymatch(df = df,
  #                                               match_type = match_type)))

  fuzzy_check2 <- check.anymatch(df = df, match_type = match_type)
  fuzzy_groups <- return.matchgroups(fuzzy_check2)

  message("Varieties left to check: ", nrow(fuzzy_groups[["check"]]))

  return(fuzzy_groups)
}

#' Create a data.frame of the names of varieties with no match
#'
#' @param df_nomatch A data.frame of varieties without cultivar matches
#'   from the database.  This will generally be the output of the function
#'   clean_manualfuzzy().
# @param variety_col_name A column name containing varieties
# @param intid_col_name A column name containing internal identifiers
#' @param ... Extra columns to select
#' @keywords internal
create_names_nomatch <- function(df_nomatch,  ...){

  dots <- enquos(...)

  nomatch1 <- df_nomatch %>%
    select(var_id,
           variety,
           intid,
           crop_type,
           type,
           !!!dots) %>%
    unique(.) %>%
    group_by(var_id)  %>% ungroup() %>%
    group_by(intid) %>%
    mutate(instance = row_number()) %>% ungroup() %>%
    arrange(crop_type, intid) %>%
    mutate(new_std_name = variety)

}



#' Combine all the matches from all of the cultivar matching steps

#' @param ls A list of results created with list2 from any of the cultivar matching steps
#' @param ... Any number of bare variables to be retained in the output of the matches
#' @keywords internal
collect_matches <- function(ls, ...){
  #temp_ls <- rlang::list2(...)
  dots <- enquos(...)
  dots_string <- purrr::map_chr(dots, ~rlang::as_name(.x))

  temp_ls2 <- ls %>%
    map(., pluck("match")) %>%
    imap(., function(x,y){ x %>% mutate(match_step = y) %>%
        # Not every step may have the columns specified by dots, so use one_of()
      select(var_id, variety, variety_db, match_step, type, crop_type, one_of(dots_string))
    })


  matches <- bind_rows(temp_ls2) %>% arrange(var_id)

}

#' Bind the status of fuzzymatches with the fuzzymatches created in this session
#' so that the two stay in sync even if the fuzzymatch function is changed
#' Note: this function may have to be updated to handle multiple auxiliary fuzzymatch files
#' @param output_fuzzymatch_df A data.frame with the newly generated fuzzymatches
#' @param aux_fuzzy_status A data.frame containing the fuzzymatches with is_truematch manually checked
#' @keywords internal
bind_fuzzymatches <- function(output_fuzzymatch_df, aux_fuzzy_status){

  output_fuzzymatch_status <-
    full_join(output_fuzzymatch_df,
              aux_fuzzy_status,
              # Need to join by as few variables as possible due to
              # changes between the output_fuzzymatch_df and aux_fuzzy_status
              # e.g. Cannot bind by db_id this variable is currently newly
              # generated on each new run (it depends if changes have
              # been made in the controlled_vocab)
              by = c("intid",
                     "intid_db",
                     "var_id",
                     "variety"),
              suffix = c("", "_status")) %>%
    relocate(contains("is_truematch")) %>%
    mutate(is_truematch = ifelse(is_truematch == "", NA, is_truematch)) %>%
    mutate(is_truematch = ifelse(is.na(is_truematch), is_truematch_status, is_truematch)) %>%
    select(is_truematch, intid, intid_db,
           variety, variety_db, var_id, db_id,
           type, type_db, crop_type)

  return(output_fuzzymatch_status)

}


