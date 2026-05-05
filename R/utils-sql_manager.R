#' Create a SQL Server Connection Using a Named Service Configuration
#'
#' Creates an ODBC connection using credentials stored in a config file.
#'
#' @param service Character name of the config section (e.g. "prod", "dev").
#' @param config_file Path to the config YAML file.
#'
#' @return A DBI connection object.
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
