#' Legacy App Login Tracking Bridge (Deprecated)
#'
#' @description Catches legacy operations calling analytic frameworks that haven't shifted to
#' the new structure yet.
#' @export
record_login <- function(user = "Guest") {
  .Deprecated(
    new = "db_record_login",
    package = "dauPortalTools",
    msg = "record_login() is deprecated; switch over to use db_record_login() which returns integer user_ids."
  )

  # Safely returns the user_id integer context for backward-compatibility support loops
  db_record_login(user = user)
}
