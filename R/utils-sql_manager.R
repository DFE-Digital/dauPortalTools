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
