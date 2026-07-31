#' Retrieve Application Users and Assigned Roles
#'
#' Returns all users along with their assigned role for the current
#' application. Users without an assigned role are returned with a
#' default role of `"User"`.
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()].
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing user information and role details.
#' @export
db_get_app_users <- function() {
  log_event("Starting db_get_app_users")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_app_users")
    },
    add = TRUE
  )

  app_id <- utils_get_app_id()

  query <- glue::glue_sql(
    "
    SELECT
      u.user_id,
      u.username,
      u.email,
      ISNULL(r.friendly_name, 'User') AS role_name,
      ur.role_id
    FROM {utils_resolve_schema('db_schema_01sr')}.[users] u
    LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] ur
      ON ur.user_id = u.user_id
     AND ur.app_id  = {app_id}
     AND ur.is_active = 1
    LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] r
      ON ur.role_id = r.role_id
    ORDER BY u.username;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}


#' Retrieve Available Roles
#'
#' Returns all roles available within the application.
#'
#' @return A [`data.frame`] with role configurations.
#' @export
db_get_roles <- function() {
  log_event("Starting db_get_roles")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_roles")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    SELECT role_id, role_name
    FROM {utils_resolve_schema('db_schema_01sr')}.[roles_config]
    ORDER BY role_name;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}


#' Update or Insert a User Role Assignment
#'
#' Assigns or updates a user's role for a given application. If a record
#' already exists, it is activated and updated; otherwise, a new record is inserted.
#'
#' @param user_id Integer scalar. Identifier of the user.
#' @param role_id Integer scalar. Identifier of the role to assign.
#' @param app_id Integer scalar. Application identifier.
#' @param assigned_by Integer scalar. Identifier of the user performing the assignment.
#' @return Numeric rows affected.
#' @export
db_update_user_role <- function(user_id, role_id, app_id, assigned_by) {
  log_event("Starting db_update_user_role")

  user_id <- as.integer(user_id)
  role_id <- as.integer(role_id)
  app_id <- as.integer(app_id)
  assigned_by <- as.integer(assigned_by)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_user_role")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    MERGE {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] AS tgt
    USING (
      SELECT
        {user_id} AS user_id,
        {role_id} AS role_id,
        {app_id}  AS app_id
    ) AS src
       ON tgt.user_id = src.user_id
      AND tgt.role_id = src.role_id
      AND tgt.app_id  = src.app_id

    WHEN MATCHED THEN
      UPDATE SET
        is_active   = 1,
        assigned_at = SYSUTCDATETIME(),
        assigned_by = {assigned_by},
        revoked_at  = NULL,
        revoked_by  = NULL

    WHEN NOT MATCHED THEN
      INSERT (user_id, role_id, app_id, is_active, assigned_by, assigned_at)
      VALUES (src.user_id, src.role_id, src.app_id, 1, {assigned_by}, SYSUTCDATETIME());
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}
