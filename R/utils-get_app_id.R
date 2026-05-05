#' Get current application ID
#'
#' Retrieves the app_id from configuration.
#'
#' @return Integer app_id
#' @export
utils_get_app_id <- function() {
  log_event("Starting utils_get_app_id")

  cfg <- get_config()

  if (is.null(cfg$app_details$app_id)) {
    log_event("utils_get_app_id error: app_id missing")
    stop("app_details$app_id is missing from configuration", call. = FALSE)
  }

  log_event(
    paste0(
      "Finished utils_get_app_id (app_id = ",
      cfg$app_details$app_id,
      ")"
    )
  )

  as.integer(cfg$app_details$app_id)
}
