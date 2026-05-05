#' Load portal configuration from YAML
#'
#' Reads a YAML config file and returns a validated configuration list.
#'
#' @param path Path to config YAML file.
#'
#' @return A structured configuration list.
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
