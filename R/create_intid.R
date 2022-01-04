#' Create internal id for variety names
#'
#' Note: This function handles aliases passed in as a separate column or aliases
#' in the same column as the variety name.  Aliases in the same column must be
#' detectable by a regex provided in sep_aliases
#'
#' @param df A data.frame to with a column containing variety names
#' @param variety_col_name A column name denoting the column containing varieties
#' @param sep_aliases A regex corresponding to the characters that are used to separate variety aliases
#' @param alias_col A column name denoting the column containing aliases
#' @family variety matching functions
#' @export
create_intid <- function(df, variety_col_name, sep_aliases = NULL,..., alias_col = NULL){

  variety_col_name <- enquo(variety_col_name)
  dots <- enquos(...)
  quo_alias <- enquo(alias_col)

  df <- df %>% rename(variety := !!variety_col_name)

  if(!rlang::quo_is_null(quo_alias)){

    df <- df %>%
      rename(alias := !!quo_alias) %>%
      mutate(alias = ifelse(variety == alias, NA, alias)) %>%
      mutate(var_id = row_number()) %>%
      pivot_longer(cols = c(variety, alias),
                   names_to = "type",
                   values_to = "variety",
                   values_drop_na = TRUE) %>%
      dplyr::select(var_id, type, variety, !!!dots)


  } else{
    df <- df %>% dplyr::select( variety, !!!dots) %>% mutate(type = "variety")
  }

  if (!is.null(sep_aliases)){

    df_variety1 <- df %>%
      mutate(intid = variety) %>%
      unique(.) %>%
      separate_rows(intid,
                    sep = sep_aliases) %>%
      # list the second listed name as the alias
      group_by(variety) %>%
      mutate(n = row_number()) %>%
      mutate(test_duptype = type == first(type)) %>%
      mutate(type = ifelse((test_duptype == TRUE & n == 2), "alias", type)) %>%
      ungroup() %>%
      select(-c(n, test_duptype))


  } else {
    df_variety1 <- df %>%
      unique(.) %>%
      mutate(intid := variety)

  }

  df_variety2 <- df_variety1 %>%
    mutate(intid = tolower(intid)) %>%
    mutate(intid = gsub("[^A-Za-z0-9+]", "", intid)) %>%
    arrange(variety) %>%
    unique(.)

  if (any(str_detect(names(df_variety2), "nursery"))){
    df_variety2 <- df_variety2 %>%
      group_by(variety, intid) %>%
      mutate(nursery = paste(unique(nursery), collapse=";")) %>%
      ungroup()

  }

  if(!"var_id" %in% names(df_variety2)){
    df_variety2 <- df_variety2 %>%
      group_by(variety) %>%
      mutate(var_id = cur_group_id()) %>%
      distinct(var_id, intid, .keep_all = TRUE) %>%
      ungroup()
  }

  df_variety2 <- df_variety2 %>% mutate(var_id = as.character(var_id))

  return(df_variety2)
}

