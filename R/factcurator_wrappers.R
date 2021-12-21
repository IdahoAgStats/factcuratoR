#' A wrapper for readin_db to maintain backwards compatibility
#'
#' @inheritParams readin_db
#' @rdname readin_db
#' @export
readin.db <- function(db_folder){
  message("This function name is being retained for backwards compatibility.
          Please use readin_db()")

  readin_db(db_folder = db_folder)

}


#' A wrapper for list_db_books to maintain backwards compatibility
#'
#' @inheritParams list_db_books
#' @rdname list_db_books
#' @export
list.db_books <- function(db_folder){
  message("This function name is being retained for backwards compatibility.
          Please use list_db_books()")

  list_db_books(db_folder = db_folder)

}


#' A wrapper for list_db_var to maintain backwards compatibility
#'
#' @inheritParams list_db_var
#' @rdname list_db_var
#' @export
list.db_var <- function(db_folder, codebook_name, required_only = FALSE){
  message("This function name is being retained for backwards compatibility.
          Please use list_db_var()")

  list_db_var(db_folder = db_folder,
              codebook_name = codebook_name,
              required_only = FALSE)

}

#' A wrapper for get_variety_db to maintain backwards compatibility
#'
#' @inheritParams get_variety_db
#' @rdname get_variety_db
#' @export
get.variety_db <- function(db_folder,
                           select_before = "2021-01-01",
                           select_crops = NULL,
                           for_matching = FALSE){
  message("This function name is being retained for backwards compatibility.
          Please use get_variety_db()")

  get_variety_db(db_folder = db_folder,
                 select_before = select_before,
                 select_crops = select_crops,
                 for_matching = for_matching)

}


#' A wrapper for create_rules to maintain backwards compatibility
#'
#' @inheritParams create_rules
#' @rdname create_rules
#' @export
create.rules <- function(df_type, db_folder){
  message("This function name is being retained for backwards compatibility.
          Please use create_rules()")

  create_rules(df_type = df_type, db_folder = db_folder)

}

