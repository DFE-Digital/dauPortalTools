#' Retrieve Application ID from Configuration
#'
#' Returns the application identifier (`app_id`) from the loaded configuration.
#'
#' @details
#' The value is read from `app_details$app_id` within the configuration object
#' returned by [get_config()]. An error is thrown if the value is missing.
#'
#' @return Integer scalar representing the application ID.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Reads configuration via [get_config()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @examples
#' \dontrun{
#' utils_get_app_id()
#' }
#'
#' @seealso [get_config()]
#'
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

  cfg$app_details$app_id
}
