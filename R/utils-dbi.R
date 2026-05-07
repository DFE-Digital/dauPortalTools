#' Execute a Database Query#' Execute a#'
#' Thin wrapper around [DBI::dbGetQuery()], used to execute a SQL query
#' and return the results as a data frame.
#'
#' @param conn A `DBIConnection` object.
#' @param query A character string or SQL object representing the query
#'   to execute.
#'
#' @details
#' This wrapper provides a consistent interface for executing read queries
#' within the package and enables safe mocking during unit testing.
#'
#' @return A `data.frame` containing the query results.
#'
#' @seealso [DBI::dbGetQuery()]
#'
#' @export
utils_db_get_query <- function(conn, query) {
  DBI::dbGetQuery(conn, query)
}


#' Execute a Database Command#' Execute a#'
#' Thin wrapper around [DBI::dbExecute()], used to execute SQL statements
#' that modify data (e.g. `INSERT`, `UPDATE`, `DELETE`).
#'
#' @param conn A `DBIConnection` object.
#' @param query A character string or SQL object representing the command
#'   to execute.
#'
#' @details
#' This wrapper standardises write operations across the package and enables
#' safe mocking during unit testing without requiring changes to function
#' signatures.
#'
#' @return Numeric scalar indicating the number of rows affected.
#'
#' @seealso [DBI::dbExecute()]
#'
#' @export
utils_db_execute <- function(conn, query) {
  DBI::dbExecute(conn, query)
}
