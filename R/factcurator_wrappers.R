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
