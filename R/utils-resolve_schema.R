#' Resolve a logical schema key to a SQL-safe identifier
#'
#' Converts an internal schema key (e.g. "01a", "01s") into a
#' SQL-safe identifier using the configuration-defined mapping.
#'
#' This function is the ONLY permitted way to introduce a schema
#' identifier into SQL.
#'
#' @param key Character scalar. Logical schema key (e.g. "01a").
#'
#' @return A \code{DBI::SQL} object suitable for use in \code{glue_sql()}.
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

  SQL(value)
}
