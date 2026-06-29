#' Create a SQL Server Connection
#'
#' Creates a database connection using configuration values stored in a
#' YAML configuration file.
#'
#' @param service Character scalar. Name of the configuration section to use
#'   (e.g. `"prod"`, `"dev"`).
#' @param config_file Character scalar. Path to the configuration file.
#'   Defaults to `"./config.yml"`.
#'
#' @details
#' The function reads connection details using [config::get()] and validates
#' that the required fields are present before creating the connection.
#'
#' Required fields:
#' \itemize{
#'   \item \code{driver}
#'   \item \code{server}
#'   \item \code{database}
#'   \item \code{uid}
#'   \item \code{pwd}
#'   \item \code{trusted}
#' }
#'
#' An error is thrown if any required configuration values are missing.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Reads configuration from disk via [config::get()]
#'   \item Establishes a live database connection via [DBI::dbConnect()]
#' }
#'
#' @return A `DBIConnection` object.
#'
#' @seealso [DBI::dbConnect()], [config::get()]
#'
#' @export

sql_manager <- function(service, config_file = "./config.yml") {
  conf <- config::get(service, file = config_file)

  required <- c("driver", "server", "database", "uid", "pwd", "trusted")
  missing <- setdiff(required, names(conf))

  if (length(missing) > 0) {
    stop(
      "Missing required DB config fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  DBI::dbConnect(
    odbc::odbc(),
    Driver = conf$driver,
    Server = conf$server,
    Database = conf$database,
    UID = conf$uid,
    PWD = conf$pwd,
    Trusted_Connection = conf$trusted
  )
}


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
