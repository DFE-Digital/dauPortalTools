#' Load application configuration from YAML
#'
#' Reads a YAML configuration file and returns a validated configuration list.
#'
#' @param path Character scalar. Path to the configuration YAML file.
#'
#' @details
#' The function reads the configuration using [yaml::read_yaml()] and
#' performs basic validation checks.
#'
#' The following fields must be present:
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
#' @return A list containing structured configuration values.
#'
#' @seealso \code{\link{utils_get_app_id}}
#'
#' @export

get_config <- function(path = "./config.yml") {
  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }

  conf <- yaml::read_yaml(path, eval.expr = TRUE)

  if (is.null(conf$app_details$app_id)) {
    stop("config.yml is missing: app_details$app_id", call. = FALSE)
  }

  if (is.null(conf$logging)) {
    stop("config.yml is missing: logging section", call. = FALSE)
  }

  conf
}
