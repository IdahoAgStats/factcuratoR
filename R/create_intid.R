#' Create variety id (var_id) internal id (intid) for variety names
#'
#' Create var_id and intid for variety names.
#' A var_id is an id (here, the row number) given to a unique variety name in the
#' raw data.
#' For example, "Variety 1/Alias 1" would be given a var_id number.
#' Let's say the var_id is 1.
#' The intid is the identifier for a name that is:
#' - all lowercase
#' - no spaces
#' - no special characters
#'
#' This function separates varieties and aliases and creates an intid for each.
#' So the resulting intids for the example above are:
#' var_id | variety           | intid
#' 1      | Variety1/Alias 1  | variety1
#' 1      | Variety1/Alias 1  | alias1
#'

#' Note: This function handles aliases passed in as a separate column or aliases
#' in the same column as the variety name.  Aliases in the same column must be
#' detectable by a regex provided in sep_aliases.
#' Also, currently, only sep_aliases OR alias_col can be provided.  (The function
#' is not written to handle aliases in both the variety column and a separate column.)
#'
#' @param df A data.frame to with a column containing variety names
#' @param variety_col_name A bare column name denoting the column containing varieties
#' @param sep_aliases A regex corresponding to the characters that are used to separate variety aliases
#' @param alias_col A bare column name denoting the column containing aliases
#' @family match variety functions
#' @export
create_intid <- function(df, variety_col_name, sep_aliases = NULL,..., alias_col = NULL){

  variety_col_name <- enquo(variety_col_name)
  dots <- enquos(...)
  quo_alias <- enquo(alias_col)

  df <- df %>% rename(variety := !!variety_col_name)

  # If aliases are provided in a separate column,
  # assign the row a var_id so that the variety and alias will be linked
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
    df <- df %>% dplyr::select(variety, !!!dots) %>% mutate(type = "variety")
  }

  # Separate a variety and alias name based on a regex separator
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

  # Create the internal id
  df_variety2 <- df_variety1 %>%
    mutate(intid = tolower(intid)) %>%
    mutate(intid = gsub("[^A-Za-z0-9+]", "", intid)) %>%
    arrange(variety) %>%
    unique(.)

  # Collapse the nursery if more than one nursery is provided for a given
  # variety and intid
  # Perhaps this code can be removed (perhaps name de-duplication and clean
  # up should be done outside of the function)
  if (any(str_detect(names(df_variety2), "nursery"))){
    df_variety2 <- df_variety2 %>%
      group_by(variety, intid) %>%
      mutate(nursery = paste(unique(nursery), collapse=";")) %>%
      ungroup()

  }

  # If the var_id hasn't been created yet, create it
  if(!"var_id" %in% names(df_variety2)){
    df_variety2 <- df_variety2 %>%
      group_by(variety) %>%
      mutate(var_id = cur_group_id()) %>%
      distinct(var_id, intid, .keep_all = TRUE) %>%
      ungroup()
  }

  # Convert the var_id to a character
  df_variety2 <- df_variety2 %>% mutate(var_id = as.character(var_id))

  return(df_variety2)
}

