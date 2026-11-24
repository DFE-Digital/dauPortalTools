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


db_get_latest_ofsted_inspection <- function(urn, publication_date = NULL) {
  .Deprecated(
    new = "db_ow_get_record",
    package = "dauPortalTools",
    msg = "db_get_latest_ofsted_inspection() is deprecated; switch over to use db_ow_get_record() which is functionally identical."
  )

  db_ow_get_record(urn, publication_date)
}

# ==============================================================================
# File: R/deprecated-db.R
# Purpose: Backward-compatibility wrappers for renamed database functions
#
# Functions:
#   - db_get_user_by_id()         -> db_users_get_by_id()
#   - db_get_user_by_email()      -> db_users_get_by_email()
#   - db_get_user_by_username()   -> db_users_get_by_username()
#   - db_get_user_email_alias()   -> db_users_get_email_alias()
#   - db_insert_new_user()        -> db_users_insert()
#   - db_update_user_email()      -> db_users_update_email()
#   - db_update_user_last_login() -> db_users_update_last_login()
#   - db_get_roles()              -> db_roles_get_all()
#   - db_get_available_roles()    -> db_roles_get_all()
#   - db_get_app_users()          -> db_roles_get_app_users()
#   - db_get_app_users_matrix()   -> db_roles_get_app_users_matrix()
#   - db_get_user_roles_by_app()  -> db_roles_get_user_roles_by_app()
#   - db_get_user_role_history()  -> db_roles_get_assignment_history()
#   - db_insert_role_assignment() -> db_roles_insert_assignment()
#   - db_revoke_role_assignment() -> db_roles_revoke_assignment()
#   - db_update_user_role()       -> db_roles_update_user_role()
# ==============================================================================

# ------------------------------------------------------------------------------
# Legacy User Functions (Migrated to R/db-users.R)
# ------------------------------------------------------------------------------

#' Legacy User Lookup by ID (Deprecated)
#' @description Deprecated wrapper for [db_users_get_by_id()].
#' @param user_id Integer scalar. Primary key identifier.
#' @return A `data.frame` containing user details, or an empty `data.frame` on error.
#' @export
db_get_user_by_id <- function(user_id) {
  .Deprecated(
    new = "db_users_get_by_id",
    package = "dauPortalTools",
    msg = "db_get_user_by_id() is deprecated; use db_users_get_by_id() instead."
  )
  db_users_get_by_id(user_id = user_id) %||% data.frame()
}

#' Legacy User Lookup by Email (Deprecated)
#' @description Deprecated wrapper for [db_users_get_by_email()].
#' @param email Character scalar. Canonical email address.
#' @return A `data.frame` containing matching user records, or an empty `data.frame`.
#' @export
db_get_user_by_email <- function(email) {
  .Deprecated(
    new = "db_users_get_by_email",
    package = "dauPortalTools",
    msg = "db_get_user_by_email() is deprecated; use db_users_get_by_email() instead."
  )
  db_users_get_by_email(email = email) %||% data.frame()
}

#' Legacy User Lookup by Username (Deprecated)
#' @description Deprecated wrapper for [db_users_get_by_username()].
#' @param username Character scalar. Legacy AD username token.
#' @return A `data.frame` containing matching user records, or an empty `data.frame`.
#' @export
db_get_user_by_username <- function(username) {
  .Deprecated(
    new = "db_users_get_by_username",
    package = "dauPortalTools",
    msg = "db_get_user_by_username() is deprecated; use db_users_get_by_username() instead."
  )
  db_users_get_by_username(username = username) %||% data.frame()
}

#' Legacy User Email Alias Lookup (Deprecated)
#' @description Deprecated wrapper for [db_users_get_email_alias()].
#' @param token Character scalar. Shorthand network username alias.
#' @return Character scalar resolved email address, or `NULL`.
#' @export
db_get_user_email_alias <- function(token) {
  .Deprecated(
    new = "db_users_get_email_alias",
    package = "dauPortalTools",
    msg = "db_get_user_email_alias() is deprecated; use db_users_get_email_alias() instead."
  )
  db_users_get_email_alias(token = token)
}

#' Legacy User Provisioning Bridge (Deprecated)
#' @description Deprecated wrapper for [db_users_insert()].
#' @param username Character scalar. Short name or token for the user.
#' @param email Character scalar. Unique canonical email address.
#' @return Integer scalar newly inserted `user_id`, or `1L` on legacy fallback.
#' @export
db_insert_new_user <- function(username, email) {
  .Deprecated(
    new = "db_users_insert",
    package = "dauPortalTools",
    msg = "db_insert_new_user() is deprecated; use db_users_insert() instead."
  )
  db_users_insert(username = username, email = email) %||% 1L
}

#' Legacy User Email Update Bridge (Deprecated)
#' @description Deprecated wrapper for [db_users_update_email()].
#' @param user_id Integer scalar. Primary key user identifier.
#' @param email Character scalar. Canonical email address.
#' @return Integer scalar rows affected.
#' @export
db_update_user_email <- function(user_id, email) {
  .Deprecated(
    new = "db_users_update_email",
    package = "dauPortalTools",
    msg = "db_update_user_email() is deprecated; use db_users_update_email() instead."
  )
  db_users_update_email(user_id = user_id, email = email)
}

#' Legacy User Last Login Update Bridge (Deprecated)
#' @description Deprecated wrapper for [db_users_update_last_login()].
#' @param user_id Integer scalar. Canonical `user_id`.
#' @return Logical scalar (invisibly).
#' @export
db_update_user_last_login <- function(user_id) {
  .Deprecated(
    new = "db_users_update_last_login",
    package = "dauPortalTools",
    msg = "db_update_user_last_login() is deprecated; use db_users_update_last_login() instead."
  )
  db_users_update_last_login(user_id = user_id)
}

# ------------------------------------------------------------------------------
# Legacy Role & Access Control Functions (Migrated to R/db-roles.R)
# ------------------------------------------------------------------------------

#' Legacy Available Roles Lookup (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_all()].
#' @return A `data.frame` with role configurations.
#' @export
db_get_roles <- function() {
  .Deprecated(
    new = "db_roles_get_all",
    package = "dauPortalTools",
    msg = "db_get_roles() is deprecated; use db_roles_get_all() instead."
  )
  db_roles_get_all() %||% data.frame()
}

#' Legacy Reference Table Role Lookup (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_all()].
#' @param conn Optional `DBIConnection` object.
#' @return A `data.frame` containing available role configurations.
#' @export
db_get_available_roles <- function(conn = NULL) {
  .Deprecated(
    new = "db_roles_get_all",
    package = "dauPortalTools",
    msg = "db_get_available_roles() is deprecated; use db_roles_get_all() instead."
  )
  db_roles_get_all(conn = conn) %||% data.frame()
}

#' Legacy Application Users Lookup (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_app_users()].
#' @param app_id Integer scalar. Application identifier. Defaults to [utils_get_app_id()].
#' @return A `data.frame` of users and assigned roles.
#' @export
db_get_app_users <- function(app_id = utils_get_app_id()) {
  .Deprecated(
    new = "db_roles_get_app_users",
    package = "dauPortalTools",
    msg = "db_get_app_users() is deprecated; use db_roles_get_app_users() instead."
  )
  db_roles_get_app_users(app_id = app_id) %||% data.frame()
}

#' Legacy Access Control Matrix Query (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_app_users_matrix()].
#' @param conn Optional `DBIConnection` object.
#' @param app_id Integer scalar. Application context filter.
#' @return A `data.frame` listing assigned users and current roles.
#' @export
db_get_app_users_matrix <- function(conn, app_id) {
  .Deprecated(
    new = "db_roles_get_app_users_matrix",
    package = "dauPortalTools",
    msg = "db_get_app_users_matrix() is deprecated; use db_roles_get_app_users_matrix() instead."
  )
  db_roles_get_app_users_matrix(app_id = app_id, conn = conn) %||% data.frame()
}

#' Legacy User Roles by App Query (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_user_roles_by_app()].
#' @param user_id Integer scalar. Resolved user profile identifier.
#' @param app_id Integer scalar. App scope key context.
#' @return Character vector of active role names.
#' @export
db_get_user_roles_by_app <- function(user_id, app_id) {
  .Deprecated(
    new = "db_roles_get_user_roles_by_app",
    package = "dauPortalTools",
    msg = "db_get_user_roles_by_app() is deprecated; use db_roles_get_user_roles_by_app() instead."
  )
  db_roles_get_user_roles_by_app(user_id = user_id, app_id = app_id)
}

#' Legacy Role Assignment Audit History Query (Deprecated)
#' @description Deprecated wrapper for [db_roles_get_assignment_history()].
#' @param app_id Integer scalar. Target application scope context.
#' @return A `data.frame` containing the assignment history ledger.
#' @export
db_get_user_role_history <- function(app_id) {
  .Deprecated(
    new = "db_roles_get_assignment_history",
    package = "dauPortalTools",
    msg = "db_get_user_role_history() is deprecated; use db_roles_get_assignment_history() instead."
  )
  db_roles_get_assignment_history(app_id = app_id) %||% data.frame()
}

#' Legacy Role Assignment Insert/Merge Bridge (Deprecated)
#' @description Deprecated wrapper for [db_roles_insert_assignment()].
#' @param conn Optional `DBIConnection` object.
#' @param user_id Integer scalar. Target user ID receiving the role.
#' @param role_id Integer scalar. ID of the role being assigned.
#' @param app_id Integer scalar. Target application ID scope.
#' @param assigned_by Integer scalar. User ID of the granting administrator.
#' @return Integer scalar rows affected.
#' @export
db_insert_role_assignment <- function(
  conn,
  user_id,
  role_id,
  app_id,
  assigned_by
) {
  .Deprecated(
    new = "db_roles_insert_assignment",
    package = "dauPortalTools",
    msg = "db_insert_role_assignment() is deprecated; use db_roles_insert_assignment() instead."
  )
  db_roles_insert_assignment(
    user_id = user_id,
    role_id = role_id,
    app_id = app_id,
    assigned_by = assigned_by,
    conn = conn
  )
}

#' Legacy Role Assignment Revoke Bridge (Deprecated)
#' @description Deprecated wrapper for [db_roles_revoke_assignment()].
#' @param conn Optional `DBIConnection` object.
#' @param user_id Integer scalar. Target user ID losing the role.
#' @param role_id Integer scalar. ID of the role being revoked.
#' @param app_id Integer scalar. Target application ID scope.
#' @param revoked_by Integer scalar. User ID of the revoking administrator.
#' @return Integer scalar rows affected.
#' @export
db_revoke_role_assignment <- function(
  conn,
  user_id,
  role_id,
  app_id,
  revoked_by
) {
  .Deprecated(
    new = "db_roles_revoke_assignment",
    package = "dauPortalTools",
    msg = "db_revoke_role_assignment() is deprecated; use db_roles_revoke_assignment() instead."
  )
  db_roles_revoke_assignment(
    user_id = user_id,
    role_id = role_id,
    app_id = app_id,
    revoked_by = revoked_by,
    conn = conn
  )
}

#' Legacy User Role Replacement Bridge (Deprecated)
#' @description Deprecated wrapper for [db_roles_update_user_role()].
#' @param user_id Integer scalar. Target user ID.
#' @param role_id Integer scalar. New role ID.
#' @param app_id Integer scalar. Application ID.
#' @param assigned_by Integer scalar. Admin user ID making the change.
#' @return Integer scalar rows affected.
#' @export
db_update_user_role <- function(user_id, role_id, app_id, assigned_by) {
  .Deprecated(
    new = "db_roles_update_user_role",
    package = "dauPortalTools",
    msg = "db_update_user_role() is deprecated; use db_roles_update_user_role() instead."
  )
  db_roles_update_user_role(
    user_id = user_id,
    role_id = role_id,
    app_id = app_id,
    assigned_by = assigned_by
  )
}

# ------------------------------------------------------------------------------
# Legacy Significant Change Functions (Migrated to R/db-sigchange.R)
# ------------------------------------------------------------------------------

#' Legacy Significant Change Query with Region Metadata (Deprecated)
#'
#' @description Deprecated wrapper for [db_sigchange_get_all()].
#' @param db_get_query Deprecated. Ignored; queries execute via [utils_db_get_query()].
#' @return A `data.frame` of significant change records with region metadata.
#' @export
db_get_sig_change_w_region <- function(db_get_query = utils_db_get_query) {
  .Deprecated(
    new = "db_sigchange_get_all",
    package = "dauPortalTools",
    msg = "db_get_sig_change_w_region() is deprecated; use db_sigchange_get_all() instead."
  )

  res <- db_sigchange_get_all()
  if (!is.null(res) && "urn" %in% names(res) && !"URN" %in% names(res)) {
    res$URN <- res$urn
  }
  res
}

# ------------------------------------------------------------------------------
# Legacy Quality Check Functions (Migrated to R/db-quality.R)
# ------------------------------------------------------------------------------

#' Legacy Active Quality Issue Query (Deprecated)
#'
#' @description Deprecated wrapper for [db_quality_get_data()].
#' @param record Integer scalar or `NULL`. Optional record ID filter.
#' @param db_get_query Deprecated. Ignored; queries execute via [utils_db_get_query()].
#' @return A `data.frame` with legacy Title Case column names.
#' @export
quality_get_data <- function(
  record = NULL,
  db_get_query = utils_db_get_query
) {
  .Deprecated(
    new = "db_quality_get_data",
    package = "dauPortalTools",
    msg = "quality_get_data() is deprecated; use db_quality_get_data() instead."
  )

  res <- db_quality_get_data(record = record)
  if (!is.null(res) && nrow(res) > 0L) {
    names(res) <- c(
      "Quality Concern",
      "Description",
      "Date Identified",
      "Last Reviewed"
    )
  }
  res
}

#' Legacy Quality Check Log Insert Bridge (Deprecated)
#'
#' @description Deprecated wrapper for [db_quality_insert_log()].
#' @param quality_check_id Integer scalar. Identifier for the quality check.
#' @param live_issues Integer scalar or `NA`. Active issue count.
#' @param new_issues Integer scalar or `NA`. New issue count.
#' @param closed_issues Integer scalar or `NA`. Closed issue count.
#' @param db_execute Deprecated. Ignored; commands execute via [utils_db_execute()].
#' @return Invisible integer scalar indicating rows affected.
#' @export
quality_add_log <- function(
  quality_check_id,
  live_issues = 0,
  new_issues = 0,
  closed_issues = 0,
  db_execute = utils_db_execute
) {
  .Deprecated(
    new = "db_quality_insert_log",
    package = "dauPortalTools",
    msg = "quality_add_log() is deprecated; use db_quality_insert_log() instead."
  )

  db_quality_insert_log(
    quality_check_id = quality_check_id,
    live_issues = live_issues,
    new_issues = new_issues,
    closed_issues = closed_issues
  )
}

#' Legacy Quality Dashboard Records Query (Deprecated)
#'
#' @description Deprecated wrapper for [db_quality_get_dashboard_records()].
#' @param app_id Integer scalar or `NULL`. Optional application ID filter.
#' @param db_get_query Deprecated. Ignored; queries execute via [utils_db_get_query()].
#' @return A `data.frame` of active quality issues for dashboard display.
#' @export
quality_get_dashboard_records <- function(
  app_id = NULL,
  db_get_query = utils_db_get_query
) {
  .Deprecated(
    new = "db_quality_get_dashboard_records",
    package = "dauPortalTools",
    msg = "quality_get_dashboard_records() is deprecated; use db_quality_get_dashboard_records() instead."
  )

  db_quality_get_dashboard_records(app_id = app_id)
}

# ------------------------------------------------------------------------------
# Legacy Portal Message Functions (Migrated to R/db-messages.R)
# ------------------------------------------------------------------------------

#' Legacy Active Portal Messages Query (Deprecated)
#' @description Deprecated wrapper for [db_messages_get_active()].
#' @return A `data.frame` containing active portal messages, or an empty `data.frame` on error.
#' @export
db_get_portal_messages <- function() {
  .Deprecated(
    new = "db_messages_get_active",
    package = "dauPortalTools",
    msg = "db_get_portal_messages() is deprecated; use db_messages_get_active() instead."
  )
  db_messages_get_active() %||% data.frame()
}

#' Legacy Add Portal Message Bridge (Deprecated)
#' @description Deprecated wrapper for [db_messages_insert()].
#' @param message_text Character scalar. Message body to display.
#' @param priority Integer scalar. Message priority. Defaults to `1`.
#' @param user_id Integer scalar. Creator user ID.
#' @param force_catch_all Logical scalar. Global catch-all flag. Defaults to `FALSE`.
#' @return Logical scalar (invisibly).
#' @export
db_add_portal_message <- function(
  message_text,
  priority = 1,
  user_id,
  force_catch_all = FALSE
) {
  .Deprecated(
    new = "db_messages_insert",
    package = "dauPortalTools",
    msg = "db_add_portal_message() is deprecated; use db_messages_insert() instead."
  )
  db_messages_insert(
    message_text = message_text,
    user_id = user_id,
    priority = priority,
    force_catch_all = force_catch_all
  )
}

#' Legacy Edit Portal Message Bridge (Deprecated)
#' @description Deprecated wrapper for [db_messages_update()].
#' @param message_id Integer scalar. Primary key identifier.
#' @param message_text Optional character scalar. Updated message body.
#' @param priority Optional integer scalar. Updated priority level.
#' @param force_catch_all Optional logical scalar. Global catch-all flag.
#' @param is_active Optional logical scalar. Visibility status.
#' @param user_id Integer scalar. Modifying administrator user ID.
#' @return Logical scalar (invisibly).
#' @export
db_edit_portal_message <- function(
  message_id,
  message_text = NULL,
  priority = NULL,
  force_catch_all = NULL,
  is_active = NULL,
  user_id
) {
  .Deprecated(
    new = "db_messages_update",
    package = "dauPortalTools",
    msg = "db_edit_portal_message() is deprecated; use db_messages_update() instead."
  )
  db_messages_update(
    message_id = message_id,
    user_id = user_id,
    message_text = message_text,
    priority = priority,
    force_catch_all = force_catch_all,
    is_active = is_active
  )
}

#' Legacy Deactivate Portal Message Bridge (Deprecated)
#' @description Deprecated wrapper for [db_messages_deactivate()].
#' @param message_id Integer scalar. Primary key identifier.
#' @param user_id Integer scalar. Deactivating administrator user ID.
#' @return Logical scalar (invisibly).
#' @export
db_deactivate_portal_message <- function(message_id, user_id) {
  .Deprecated(
    new = "db_messages_deactivate",
    package = "dauPortalTools",
    msg = "db_deactivate_portal_message() is deprecated; use db_messages_deactivate() instead."
  )
  db_messages_deactivate(
    message_id = message_id,
    user_id = user_id
  )
}

# ------------------------------------------------------------------------------
# Legacy App Analytics & Audit Log Functions (Migrated to R/db-analytics.R)
# ------------------------------------------------------------------------------

#' Legacy Clickstream Telemetry Insert Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_insert()].
#' @param user_id Integer scalar. Canonical user ID.
#' @param app_id Integer scalar. Application ID.
#' @param env_id Integer scalar. Environment ID.
#' @param page_name Character scalar. View/page name.
#' @param action_type Character scalar. Action verb.
#' @param action_sub_type Optional character scalar. Supplemental detail.
#' @return Integer scalar rows affected.
#' @export
db_insert_app_analytics <- function(
  user_id,
  app_id,
  env_id,
  page_name,
  action_type,
  action_sub_type = NULL
) {
  .Deprecated(
    new = "db_analytics_insert",
    package = "dauPortalTools",
    msg = "db_insert_app_analytics() is deprecated; use db_analytics_insert() instead."
  )
  db_analytics_insert(
    user_id = user_id,
    page_name = page_name,
    action_type = action_type,
    action_sub_type = action_sub_type,
    app_id = app_id,
    env_id = env_id
  )
}

#' Legacy Download Telemetry Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_record_download()].
#' @param user Character scalar. Username or email token. Defaults to `"Guest"`.
#' @param page_name Character scalar. Page where download occurred.
#' @param file_name Character scalar. Downloaded file name.
#' @return `NULL`, invisibly.
#' @export
db_record_download <- function(user = "Guest", page_name, file_name) {
  .Deprecated(
    new = "db_analytics_record_download",
    package = "dauPortalTools",
    msg = "db_record_download() is deprecated; use db_analytics_record_download() instead."
  )
  db_analytics_record_download(
    page_name = page_name,
    file_name = file_name,
    user = user
  )
  invisible(NULL)
}

#' Legacy App Download Tracking Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_record_download()].
#' @param user Character scalar. Username or email token. Defaults to `"Guest"`.
#' @param page_name Character scalar. Page where download occurred.
#' @param file_name Character scalar. Downloaded file name.
#' @return `NULL`, invisibly.
#' @export
record_download <- function(user = "Guest", page_name, file_name) {
  .Deprecated(
    new = "db_analytics_record_download",
    package = "dauPortalTools",
    msg = "record_download() is deprecated; use db_analytics_record_download() instead."
  )
  db_analytics_record_download(
    page_name = page_name,
    file_name = file_name,
    user = user
  )
  invisible(NULL)
}

#' Legacy Login Telemetry Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_record_login()].
#' @param user Character scalar. Username or email token. Defaults to `"Guest"`.
#' @return Integer scalar `user_id`, or `NULL` on error.
#' @export
db_record_login <- function(user = "Guest") {
  .Deprecated(
    new = "db_analytics_record_login",
    package = "dauPortalTools",
    msg = "db_record_login() is deprecated; use db_analytics_record_login() instead."
  )
  db_analytics_record_login(user = user)
}

#' Legacy App Login Tracking Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_record_login()].
#' @param user Character scalar. Username or email token. Defaults to `"Guest"`.
#' @return Integer scalar `user_id`, or `NULL` on error.
#' @export
record_login <- function(user = "Guest") {
  .Deprecated(
    new = "db_analytics_record_login",
    package = "dauPortalTools",
    msg = "record_login() is deprecated; use db_analytics_record_login() instead."
  )
  db_analytics_record_login(user = user)
}

#' Legacy System Audit Log Writer (Deprecated)
#' @description Deprecated wrapper for [db_analytics_insert_audit_log()].
#' @param user_id Integer scalar. Canonical user ID executing the action.
#' @param action_type Character scalar. Action classification.
#' @param target_table Character scalar. Physical database table altered.
#' @param record_id Optional character or integer scalar. Primary key identifier.
#' @param action_summary Character scalar. Narrative summary of the change.
#' @return Integer scalar rows affected (`0L` on error).
#' @export
db_write_audit_log <- function(
  user_id,
  action_type,
  target_table,
  record_id = NULL,
  action_summary
) {
  .Deprecated(
    new = "db_analytics_insert_audit_log",
    package = "dauPortalTools",
    msg = "db_write_audit_log() is deprecated; use db_analytics_insert_audit_log() instead."
  )
  db_analytics_insert_audit_log(
    user_id = user_id,
    action_type = action_type,
    target_table = target_table,
    action_summary = action_summary,
    record_id = record_id
  )
}

#' Legacy Platform System Audit Log Insert Bridge (Deprecated)
#' @description Deprecated wrapper for [db_analytics_insert_audit_log()].
#' @param conn Optional `DBIConnection` object.
#' @param app_id Integer scalar. Application ID.
#' @param user_id Integer scalar. Canonical user ID.
#' @param env_id Integer scalar. Environment ID.
#' @param action_type Character scalar. Database mutation operation.
#' @param target_table Character scalar. Physical table modified.
#' @param record_id Optional character or integer scalar. Altered row primary key.
#' @param action_summary Character scalar. Narrative summary.
#' @return Integer scalar rows affected (`0L` on error).
#' @export
db_insert_audit_log <- function(
  conn = NULL,
  app_id,
  user_id,
  env_id,
  action_type,
  target_table,
  record_id = NULL,
  action_summary
) {
  .Deprecated(
    new = "db_analytics_insert_audit_log",
    package = "dauPortalTools",
    msg = "db_insert_audit_log() is deprecated; use db_analytics_insert_audit_log() instead."
  )
  db_analytics_insert_audit_log(
    user_id = user_id,
    action_type = action_type,
    target_table = target_table,
    action_summary = action_summary,
    record_id = record_id,
    app_id = app_id,
    env_id = env_id,
    conn = conn
  )
}

#' Legacy Aggregated Application Usage Summary Query (Deprecated)
#' @description Deprecated wrapper for [db_analytics_get_summary()].
#' @param conn Optional `DBIConnection` object.
#' @param app_id Integer scalar. Target application ID.
#' @param days_back Integer scalar. Trailing lookback window in days. Defaults to `30`.
#' @return A `data.frame` with usage summary metrics, or an empty `data.frame` on error.
#' @export
db_get_analytics_summary <- function(conn = NULL, app_id, days_back = 30) {
  .Deprecated(
    new = "db_analytics_get_summary",
    package = "dauPortalTools",
    msg = "db_get_analytics_summary() is deprecated; use db_analytics_get_summary() instead."
  )
  db_analytics_get_summary(
    app_id = app_id,
    days_back = days_back,
    conn = conn
  ) %||%
    data.frame()
}
