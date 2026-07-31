#' Resolve Environment Context Name to Integer Key
#'
#' Evaluates the application host URL strings to determine the active running environment.
#'
#' @return Integer scalar matching environments_config map (1 = dev, 2 = test, 3 = prod)
#' @keywords internal
utils_resolve_env_id <- function() {
  log_event("Starting utils_resolve_env_id")

  active_env <- Sys.getenv("R_CONFIG_ACTIVE", "default")

  host_url <- tryCatch(
    shiny::getShinyOption("app_url"),
    error = function(e) {
      NULL
    }
  )

  if (is.null(host_url) || grepl("localhost|127.0.0.1", host_url)) {
    log_event("Environment resolved to Local Development (1L).")
    return(1L)
  } else if (grepl("test|staging", host_url)) {
    log_event("Environment resolved to Test/Staging (2L).")
    return(2L)
  }

  log_event("Environment resolved to Production (3L).")
  return(3L)
}
