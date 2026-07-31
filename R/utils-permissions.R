#' Check Session Authorization Permission Flag
#'
#' Evaluates if the current user session holds authorization to execute an operation.
#'
#' @param user_id Integer. Resolved user profile identifier.
#' @param permission_name Character scalar. The specific feature flag string to check.
#' @return Logical scalar. TRUE if authorized, FALSE otherwise.
#' @export
user_has_permission <- function(user_id, permission_name) {
  log_event("Starting user_has_permission check")
  on.exit(log_event("Finished user_has_permission check"), add = TRUE)

  if (is.null(user_id) || identical(user_id, 1L)) {
    log_event(
      "Null or guest user_id evaluated. Denying permission string lookups by default."
    )
    return(FALSE)
  }

  app_id <- utils_get_app_id()
  log_event(sprintf(
    "Evaluating permission '%s' for user_id %d within app_id %d",
    permission_name,
    user_id,
    app_id
  ))

  # Low-level permissions fetcher manages its own database handle internally
  permissions <- db_get_user_permissions(user_id = user_id, app_id = app_id)

  is_authorized <- permission_name %in% permissions
  log_event(sprintf("Authorization resolution result: %s", is_authorized))

  return(is_authorized)
}
