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
