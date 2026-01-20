#' Create a SQL Server Connection Using a Named Service Configuration
#'
#' Establishes a database connection to a SQL Server instance using details
#' stored in a `config.yml` file.
#' The function loads configuration values for the specified service name and
#' uses them to create an ODBC connection via `DBI::dbConnect()`.
#'
#' @param service String. The name of the configuration block in `config.yml`
#' that contains the connection details (e.g. `"prod"`, `"dev"`, `"test"`).
#'
#' @return
#' A DBI connection object created with `DBI::dbConnect()`, which can be used
#' with standard DBI functions such as `DBI::dbGetQuery()`,
#' `DBI::dbWriteTable()`, and `DBI::dbDisconnect()`.
#'
#' @details
#' The function expects a `config.yml` file located in the working directory.
#' Each configuration block should define the fields:
#' `driver`, `server`, `database`, `uid`, `pwd`, and `trusted`.
#'
#' Example `config.yml` structure:
#' ```
#' prod:
#'   driver: "SQL Server"
#'   server: "myserver.database.windows.net"
#'   database: "mydatabase"
#'   uid: "username"
#'   pwd: "password"
#'   trusted: "False"
#' ```
#'
#' @examples
#' \dontrun{
#' # Connect to production database
#' conn <- sql_manager("prod")
#'
#' # Run a query
#' DBI::dbGetQuery(conn, "SELECT TOP 10 * FROM mytable")
#'
#' # Disconnect when finished
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
sql_manager <- function(service) {
  config <- config::get(
    paste0(service),
    file = "./config.yml"
  )

  server <- DBI::dbConnect(
    odbc::odbc(),
    Driver = config$driver,
    Server = config$server,
    Database = config$database,
    UID = config$uid,
    PWD = config$pwd,
    Trusted_Connection = config$trusted
  )
  return(server)
}
