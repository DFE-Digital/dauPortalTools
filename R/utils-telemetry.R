#' Record a Page View Interaction
#'
#' High-level wrapper tracking internal page switches (tabs, menus) into app_analytics.
#'
#' @param user_id Integer. The executing session profile key.
#' @param page_name Character scalar. Target display location context.
#' @return `NULL`, invisibly.
#' @export
log_page_view <- function(user_id, page_name) {
  log_event("Starting log_page_view orchestrator")
  on.exit(log_event("Finished log_page_view orchestrator"), add = TRUE)

  shiny::req(user_id, page_name)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()

  log_event(sprintf(
    "Logging view event for user_id %d on page '%s' under app %d",
    user_id,
    page_name,
    app_id
  ))

  # Low-level tracker opens and closes its own connection handle internally
  db_insert_app_analytics(
    user_id = user_id,
    app_id = app_id,
    env_id = env_id,
    page_name = page_name,
    action_type = "View",
    action_sub_type = "Tab Switch"
  )

  invisible(NULL)
}

#' Record a Target User Event UI Click
#'
#' High-level wrapper tracking custom actions like download activations or filter fires.
#'
#' @param user_id Integer. The executing session profile key.
#' @param page_name Character scalar. Source location visibility frame.
#' @param action_type Character scalar. Standard tracking verb ('Click', 'Download').
#' @param element_id Character scalar. Specific element identifier token.
#' @return `NULL`, invisibly.
#' @export
log_user_action <- function(user_id, page_name, action_type, element_id) {
  log_event("Starting log_user_action orchestrator")
  on.exit(log_event("Finished log_user_action orchestrator"), add = TRUE)

  shiny::req(user_id, page_name, action_type, element_id)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()

  log_event(sprintf(
    "Logging UI action [%s] on element '%s' (Page: '%s') for user_id %d",
    action_type,
    element_id,
    page_name,
    user_id
  ))

  # Low-level tracker opens and closes its own connection handle internally
  db_insert_app_analytics(
    user_id = user_id,
    app_id = app_id,
    env_id = env_id,
    page_name = page_name,
    action_type = action_type,
    action_sub_type = element_id
  )

  invisible(NULL)
}
