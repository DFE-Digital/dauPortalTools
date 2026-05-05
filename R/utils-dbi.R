#' Execute a database query and return results
#'
#' Thin wrapper around DBI::dbGetQuery().
#' Required to safely unit-test DB code.
#'
#' @export
utils_db_get_query <- function(conn, query) {
  DBI::dbGetQuery(conn, query)
}

#' Execute a database command
#'
#' Thin wrapper around DBI::dbExecute().
#' Required to safely unit-test DB code.
#'
#' @export
utils_db_execute <- function(conn, query) {
  DBI::dbExecute(conn, query)
}
