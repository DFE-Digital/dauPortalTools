#' Get the current portal username
#'
#' Fetches the username from the active Shiny/Posit session. If the session
#' has no user (e.g., local dev), falls back to "admin".
#'
#' @param session Optional shiny session; if NULL, we try to detect it.
#' @param fallback Local-testing fallback username (default "admin").
#' @return Character scalar username.
#' @export

get_user <- function(session = NULL, fallback = "admin") {
  if (is.null(session)) {
    session <- tryCatch(
      shiny::getDefaultReactiveDomain(),
      error = function(e) NULL
    )
  }

  if (!is.null(session) && !is.null(session$user) && nzchar(session$user)) {
    return(session$user)
  }

  emulate_user <- tryCatch(
    config::get("emulate_user"),
    error = function(e) NULL
  )

  if (!is.null(emulate_user) && nzchar(emulate_user)) {
    return(emulate_user)
  }

  fallback
}
