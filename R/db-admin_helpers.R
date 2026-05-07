#' Retrieve Application Users and Assigned Roles#' Retrieve'
#' Returns all users along with their assigned role for the current
#' application. Users without an assigned role are returned with a
#' default role of `"User"`.
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()].
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' The query performs a left join between:
#' \itemize{
#'   \item `users`
#'   \item `user_roles` (filtered to the current application)
#'   \item `roles`
#' }
#'
#' Results are ordered alphabetically by username.
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start
#' and end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return A [`data.frame`] with the following columns:
#' \describe{
#'   \item{user_id}{Integer user identifier.}
#'   \item{username}{Character username.}
#'   \item{email}{Character email address.}
#'   \item{role_name}{Character role name (defaults to `"User"` if none assigned).}
#'   \item{role_id}{Integer role identifier (may be `NA`).}
#' }
#'
#' @examples
#' \dontrun{
#' users <- db_get_app_users()
#'
#' # View users
#' head(users)
#' }
#'
#' @seealso [db_get_roles()], [db_update_user_role()]
#' @export

db_get_app_users <- function() {
  log_event("Starting db_get_app_users")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_app_users")
    },
    add = TRUE
  )

  app_id <- utils_get_app_id()
  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT
      u.user_id,
      u.username,
      u.email,
      ISNULL(r.role_name, 'User') AS role_name,
      ur.role_id
    FROM {schema}.[users] u
    LEFT JOIN {schema}.[user_roles] ur
      ON ur.user_id = u.user_id
     AND ur.app_id  = {app_id}
    LEFT JOIN {schema}.[roles] r
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
#' @details
#' The database schema is resolved using [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' Results are ordered alphabetically by role name.
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start
#' and end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return A [`data.frame`] with the following columns:
#' \describe{
#'   \item{role_id}{Integer role identifier.}
#'   \item{role_name}{Character role name.}
#' }
#'
#' @examples
#' \dontrun{
#' roles <- db_get_roles()
#'
#' # View roles
#' roles
#' }
#'
#' @seealso [db_get_app_users()], [db_update_user_role()]
#' @export
db_get_roles <- function() {
  log_event("Starting db_get_roles")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_roles")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT role_id, role_name
    FROM {utils_resolve_schema('db_schema_01a')}.[roles]
    ORDER BY role_name;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}


#' Update or Insert a User Role Assignment
#'
#' Assigns or updates a user's role for a given application. If a record
#' already exists, it is updated; otherwise, a new record is inserted.
#'
#' @param user_id Integer scalar. Identifier of the user.
#' @param role_id Integer scalar. Identifier of the role to assign.
#' @param app_id Integer scalar. Application identifier.
#' @param assigned_by Integer scalar. Identifier of the user performing
#'   the assignment.
#'
#' @details
#' This function uses a SQL `MERGE` statement to:
#' \itemize{
#'   \item update existing user-role assignments, or
#'   \item insert a new assignment if none exists.
#' }
#'
#' The database schema is resolved using [utils_resolve_schema()], and
#' the query is executed using [utils_db_execute()].
#'
#' Inputs are coerced to integers prior to execution.
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start
#' and end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Writes to the database via an `INSERT` or `UPDATE`.
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return Invisible logical `TRUE` on successful execution.
#'
#' @examples
#' \dontrun{
#' db_update_user_role(
#'   user_id = 1,
#'   role_id = 2,
#'   app_id = 7,
#'   assigned_by = 3
#' )
#' }
#'
#' @seealso [db_get_app_users()], [db_get_roles()]
#' @export
db_update_user_role <- function(
  user_id,
  role_id,
  app_id,
  assigned_by
) {
  log_event("Starting db_update_user_role")

  user_id <- as.integer(user_id)
  role_id <- as.integer(role_id)
  app_id <- as.integer(app_id)
  assigned_by <- as.integer(assigned_by)

  conn <- sql_manager("dit")

  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_user_role")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    MERGE {utils_resolve_schema('db_schema_01a')}.[user_roles] AS tgt
    USING (
      SELECT
        {user_id} AS user_id,
        {role_id} AS role_id,
        {app_id}  AS app_id
    ) AS src
       ON tgt.user_id = src.user_id
      AND tgt.app_id  = src.app_id

    WHEN MATCHED THEN
      UPDATE SET
        role_id     = src.role_id,
        assigned_at = SYSUTCDATETIME(),
        assigned_by = {assigned_by}

    WHEN NOT MATCHED THEN
      INSERT (user_id, role_id, app_id, assigned_by)
      VALUES (src.user_id, src.role_id, src.app_id, {assigned_by});
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}
