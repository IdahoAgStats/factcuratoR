#' Do the steps in the exact match
#' 
#' @param data_intid A data.frame created by the function create_intid
#' @param data_suffix A character suffix to be appended to the columns related to the original data
#' @param match_type A string, either "raw" or "db, " which denotes the type of
#' matching procedure to use.  If matching raw variety names, use "raw."
#' If matching database names, use "db."  The difference is that "raw" groups by
#' the var_id and is only looking for one match per var_id.  The method "db"
#' groups by the intid and is looking to match every entry provided.
#' @inheritParams check.anymatch
#' @param rename_df A logical denoting whether to check the rename (misspelling) file
#' Otherwise, the matches are pulled from the database list
do_exactmatch <- function(data_intid,
                          select_before = "2021-01-01", 
                          select_crops = NULL,
                          match_type = "raw", 
                          rename_df = FALSE){
  if (rename_df) {
    variety_intid_db <- get_cultivar_rename() %>% 
      select(-c(crop_type, crop_type_db))
    
  } else {
  
    variety_intid_db <- get.variety_db(select_before = select_before, 
                                     select_crops = select_crops,
                                     for_matching = TRUE)
  }
  
  exact1 <- left_join(data_intid, 
                      variety_intid_db, 
                     # It is possible to join by "crop_type" = "crop_type_db"
                     # to ensure that the crop matches, however,
                     # this is currently onlyan issue for the cultivar
                     # 'Salute' and it is better not to implement because sometimes
                     # the crop_type is NA and then wouldn't join
                      by = c("intid"), 
                      suffix = c("", "_db")) 
  # This is a possible start of how to implement filtering out 
  # crop_type mismatch (see above), which needs to take into account not
  # removing the entry.  However, if a duplicate entry is created, it does need to be removed
  #  mutate(across(contains("db"), 
  #               ~ ifelse(crop_type != crop_type_db, NA,.x)))
  
  if (match_type == "raw"){
    return_unique = sym("var_id")
  } else if (match_type == "db"){
    return_unique = sym("intid")
  }
  
  exact2 <- check.anymatch(exact1, checkfor = variety_db, match_type = match_type)
  exact <- return.matchgroups(exact2)
  
  
  test_exact <- check.matches(exact[[1]])
  if (nrow(test_exact) > 0) { 
    warning("There is more that one database match, 
          please resolve in the controlled vocabularies")}
  
  message(paste(c("match:", " nomatch:", " check:", " not_needed:"), 
                paste(map(exact, ~nrow(.x)), sep = ",")))
  
  
  return(exact)
  
  
  
  
}


#' Get cultivar rename list to match.  The cv_rename.csv is located
#' in the controlled vocabularies.
get_cultivar_rename <- function(){
  db <- readin.db()
  
  cv_rename <- db$cv_rename.csv %>%
    rename(variety_db = correct_variety_name) %>% select(-Program)
  
  cv_rename2 = cv_rename %>% 
    mutate(intid = gsub("[^A-Za-z0-9+]", "", wrong_name)) %>% # will remove all characters not specified , which is needed to remove the \ backslash in one of the names
    mutate(intid = tolower(intid)) %>%
    unique(.) %>% 
    mutate(db_id = intid) %>%
    mutate(crop_type_db = NA) %>%
    mutate(type_db = NA)
  
  return(cv_rename2)
}