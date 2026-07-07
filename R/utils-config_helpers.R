#' Load application configuration from YAML
#'
#' Reads a YAML configuration file, resolves the active environment, and
#' returns a validated configuration list.
#'
#' @param path Character scalar. Path to the configuration YAML file.
#'
#' @details
#' The function reads the configuration using [config::get()] to handle
#' environment resolution (e.g., default, staging, production) and
#' performs basic validation checks.
#'
#' The following fields must be present in the resolved configuration:
#' \itemize{
#'   \item \code{app_details$app_id}
#'   \item \code{logging}
#' }
#'
#' An error is thrown if:
#' \itemize{
#'   \item The file does not exist
#'   \item Required configuration fields are missing
#' }
#'
#' @return A list containing structured, environment-specific configuration values.
#'
#' @seealso \code{\link{utils_get_app_id}}
#'
#' @export
get_config <- function(path = "./config.yml") {
  log_event("Running get_config", TRUE)

  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }

  log_event(Sys.getenv("R_CONFIG_ACTIVE"), TRUE)

  conf <- config::merge(
    config::get(file = path, config = "global"),
    config::get(file = path, config = "R_CONFIG_ACTIVE")
  )

  if (is.null(conf$app_details$app_id)) {
    stop("Configuration is missing: app_details$app_id", call. = FALSE)
  }

  if (is.null(conf$logging)) {
    stop("Configuration is missing: logging section", call. = FALSE)
  }

  log_event(conf, TRUE)

  conf
}
