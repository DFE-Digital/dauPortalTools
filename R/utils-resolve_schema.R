#' Resolve Schema Key to SQL Identifier
#'
#' Converts a logical schema key into a SQL-safe identifier using the
#' configuration-defined schema mapping.
#'
#' @param key Character scalar. Schema key used to look up the configured
#'   schema name (e.g. `"db_schema_01a"`).
#'
#' @details
#' The function retrieves schema mappings from the configuration returned
#' by [get_config()] and returns a `DBI::SQL` object suitable for use in
#' [glue::glue_sql()].
#'
#' An error is thrown if:
#' \itemize{
#'   \item \code{key} is not a valid character scalar
#'   \item The configuration is missing a \code{schemas} section
#'   \item The supplied key is not present in the configuration
#' }
#'
#' This function should be used whenever schema identifiers are inserted
#' into SQL queries to ensure consistency and safety.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Reads configuration via [get_config()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A `DBI::SQL` object representing the resolved schema name.
#'
#' @seealso [get_config()], [glue::glue_sql()]
#'
#' @export

utils_resolve_schema <- function(key) {
  log_event("Starting utils_resolve_schema")

  if (length(key) != 1 || !is.character(key) || is.na(key)) {
    log_event("utils_resolve_schema error: invalid schema key input")
    stop("Schema key must be a non-NA character scalar", call. = FALSE)
  }

  cfg <- get_config()

  if (is.null(cfg$schemas) || !is.list(cfg$schemas)) {
    log_event("utils_resolve_schema error: schemas section missing from config")
    stop("Configuration is missing 'schemas' section", call. = FALSE)
  }

  value <- cfg$schemas[[key]]

  if (is.null(value)) {
    log_event(paste0(
      "utils_resolve_schema error: unknown schema key '",
      key,
      "'"
    ))
    stop(sprintf("Unknown schema key '%s'", key), call. = FALSE)
  }

  log_event(paste0(
    "utils_resolve_schema resolved schema key '",
    key,
    "'"
  ))

  DBI::SQL(value)
}
