#' A wrapper for readin_db to maintain backwards compatibility
#'
#' @inheritParams readin_db
#' @family function rename wrappers
#' @seealso \code{\link{readin_db}}
#' @export
readin.db <- function(db_folder){
  message("This function name is being retained for backwards compatibility.
          Please use readin_db()")

  readin_db(db_folder = db_folder)

}
