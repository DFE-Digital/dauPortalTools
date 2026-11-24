# ==============================================================================
# File: R/db-roles.R
# Purpose: Database query and mutation functions for RBAC roles & assignments
#
# Functions:
#   - db_roles_get_all()                 : Fetch all configured system roles
#   - db_roles_get_app_users()           : Fetch all users with effective app role
#   - db_roles_get_app_users_matrix()    : Fetch active role assignment directory
#   - db_roles_get_user_roles_by_app()   : Fetch active role names for a user/app
#   - db_roles_get_assignment_history()  : Fetch full role assignment audit trail
#   - db_roles_insert_assignment()       : Grant or reactivate a user role (MERGE)
#   - db_roles_update_user_role()        : Replace active user role atomically
#   - db_roles_revoke_assignment()       : Soft-delete an active role assignment
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all available system roles
#'
#' Retrieves the complete reference lookup of configured roles from
#' `[roles_config]`, ordered alphabetically by `friendly_name`.
#'
#' @details
#' Queries `[roles_config]` within `utils_resolve_schema("db_schema_01sr")`.
#' Supports passing an existing `DBIConnection` via `conn` for transactional
#' reuse; otherwise opens and closes a connection via [sql_manager()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 3 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`role_id`}{Integer. Primary key role identifier.}
#'     \item{`role_name`}{Character. Internal programmatic role key.}
#'     \item{`friendly_name`}{Character. Human-readable role label for UI display.}
#'   }
#'
#' @family roles database functions
#' @seealso [db_roles_get_app_users()], [db_roles_get_app_users_matrix()],
#'   [sql_manager()], [utils_resolve_schema()]
#'
#' @export
#' @examples
#' \dontrun{
#' roles <- db_roles_get_all()
#' }
db_roles_get_all <- function(conn = NULL, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        SELECT
          [role_id],
          [role_name],
          [friendly_name]
        FROM {`schema`}.[roles_config]
        ORDER BY [friendly_name];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch all users and their effective role for an application
#'
#' Returns all users from `[users]` left-joined against active assignments in
#' `[user_role_assignments]` for `app_id`. Users without an explicit active
#' assignment default to `"User"`.
#'
#' @details
#' Queries `[users]`, `[user_role_assignments]`, and `[roles_config]` within
#' `utils_resolve_schema("db_schema_01sr")`. Defaults `app_id` to
#' [utils_get_app_id()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` ordered by `username` with 6 columns, or `NULL` on
#'   invalid input or database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Network/AD username.}
#'     \item{`email`}{Character. Canonical corporate email address.}
#'     \item{`role_id`}{Integer. Assigned role ID (`NA` if unassigned).}
#'     \item{`role_name`}{Character. Internal role key (`NA` if unassigned).}
#'     \item{`friendly_name`}{Character. Display role label, defaulting to `"User"`.}
#'   }
#'
#' @family roles database functions
#' @seealso [db_roles_get_app_users_matrix()], [db_roles_get_all()],
#'   [utils_get_app_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' app_users <- db_roles_get_app_users()
#' }
db_roles_get_app_users <- function(
  app_id = utils_get_app_id(),
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- suppressWarnings(as.integer(app_id))
  if (length(aid) != 1L || is.na(aid)) {
    log_event(paste0(fn_name, " failed: invalid app_id"), type = 2)
    return(NULL)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          u.[user_id],
          u.[username],
          u.[email],
          ur.[role_id],
          r.[role_name],
          ISNULL(r.[friendly_name], 'User') AS [friendly_name]
        FROM {`schema`}.[users] u
        LEFT JOIN {`schema`}.[user_role_assignments] ur
          ON ur.[user_id] = u.[user_id]
         AND ur.[app_id] = {aid}
         AND ur.[is_active] = 1
        LEFT JOIN {`schema`}.[roles_config] r
          ON ur.[role_id] = r.[role_id]
        ORDER BY u.[username];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch active role assignment directory matrix for an application
#'
#' Returns users who have an explicit active role assignment in
#' `[user_role_assignments]` for `app_id`, including who granted the role and
#' when.
#'
#' @details
#' Performs `INNER JOIN`s across `[user_role_assignments]`, `[users]`, and
#' `[roles_config]` within `utils_resolve_schema("db_schema_01sr")`. Supports
#' passing an existing `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 6 columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Assigned user's network username.}
#'     \item{`email`}{Character. Assigned user's email address.}
#'     \item{`assigned_role`}{Character. Friendly name of the active role.}
#'     \item{`assigned_at`}{POSIXct. UTC timestamp when the role was granted.}
#'     \item{`assigned_by_user`}{Character. Username of the administrator who granted the role.}
#'   }
#'
#' @family roles database functions
#' @seealso [db_roles_get_app_users()], [db_roles_get_assignment_history()]
#'
#' @export
#' @examples
#' \dontrun{
#' matrix_df <- db_roles_get_app_users_matrix(app_id = 1L)
#' }
db_roles_get_app_users_matrix <- function(
  app_id = utils_get_app_id(),
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- suppressWarnings(as.integer(app_id))
  if (length(aid) != 1L || is.na(aid)) {
    log_event(paste0(fn_name, " failed: invalid app_id"), type = 2)
    return(NULL)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        SELECT
          u.[user_id],
          u.[username],
          u.[email],
          rc.[friendly_name] AS [assigned_role],
          a.[assigned_at],
          admin.[username] AS [assigned_by_user]
        FROM {`schema`}.[user_role_assignments] a
        INNER JOIN {`schema`}.[users] u
          ON a.[user_id] = u.[user_id]
        INNER JOIN {`schema`}.[roles_config] rc
          ON a.[role_id] = rc.[role_id]
        INNER JOIN {`schema`}.[users] admin
          ON a.[assigned_by] = admin.[user_id]
        WHERE a.[app_id] = {aid}
          AND a.[is_active] = 1;
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch active role names for a specific user and application
#'
#' Retrieves a character vector of programmatic `role_name` values currently
#' active for `user_id` within `app_id`.
#'
#' @details
#' Queries `[user_role_assignments]` joined to `[roles_config]` within
#' `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Resolved user profile identifier.
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Character vector of active `role_name` strings, or `character(0)` if
#'   no active roles exist, inputs are invalid, or a database error occurs.
#'
#' @family roles database functions
#' @seealso [db_roles_get_all()], [db_roles_insert_assignment()]
#'
#' @export
#' @examples
#' \dontrun{
#' roles <- db_roles_get_user_roles_by_app(user_id = 42L, app_id = 1L)
#' }
db_roles_get_user_roles_by_app <- function(
  user_id,
  app_id = utils_get_app_id(),
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  aid <- suppressWarnings(as.integer(app_id))
  if (length(uid) != 1L || is.na(uid) || length(aid) != 1L || is.na(aid)) {
    log_event(paste0(fn_name, " failed: invalid user_id or app_id"), type = 2)
    return(character(0))
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT r.[role_name]
        FROM {`schema`}.[user_role_assignments] a
        INNER JOIN {`schema`}.[roles_config] r
          ON a.[role_id] = r.[role_id]
        WHERE a.[user_id] = {uid}
          AND a.[app_id] = {aid}
          AND a.[is_active] = 1;
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, query)
      if (!is.null(res) && nrow(res) > 0L) {
        return(as.character(res$role_name))
      }
      character(0)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      character(0)
    }
  )
}

#' Fetch full role assignment audit history for an application
#'
#' Returns a chronological audit ledger of all role assignments (both active
#' and revoked) for `app_id`, ordered most recent event first.
#'
#' @details
#' Queries `[user_role_assignments]` within
#' `utils_resolve_schema("db_schema_01sr")`, ordering by
#' `COALESCE(revoked_at, assigned_at) DESC`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 7 columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`target_user`}{Character. Username of the user assigned the role.}
#'     \item{`role_name`}{Character. Friendly name of the role.}
#'     \item{`is_active`}{Logical/Integer. `1`/`TRUE` if active, `0`/`FALSE` if revoked.}
#'     \item{`assigned_at`}{POSIXct. UTC timestamp when the role was assigned.}
#'     \item{`assigned_by_user`}{Character. Username of the granting administrator.}
#'     \item{`revoked_at`}{POSIXct. UTC timestamp when revoked (`NA` if active).}
#'     \item{`revoked_by_user`}{Character. Username of the revoking administrator (`NA` if active).}
#'   }
#'
#' @family roles database functions
#' @seealso [db_roles_get_app_users_matrix()], [db_roles_revoke_assignment()]
#'
#' @export
#' @examples
#' \dontrun{
#' history <- db_roles_get_assignment_history(app_id = 1L)
#' }
db_roles_get_assignment_history <- function(
  app_id = utils_get_app_id(),
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- suppressWarnings(as.integer(app_id))
  if (length(aid) != 1L || is.na(aid)) {
    log_event(paste0(fn_name, " failed: invalid app_id"), type = 2)
    return(NULL)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          u.[username] AS [target_user],
          rc.[friendly_name] AS [role_name],
          a.[is_active],
          a.[assigned_at],
          admin_in.[username] AS [assigned_by_user],
          a.[revoked_at],
          admin_out.[username] AS [revoked_by_user]
        FROM {`schema`}.[user_role_assignments] a
        INNER JOIN {`schema`}.[users] u
          ON a.[user_id] = u.[user_id]
        INNER JOIN {`schema`}.[roles_config] rc
          ON a.[role_id] = rc.[role_id]
        INNER JOIN {`schema`}.[users] admin_in
          ON a.[assigned_by] = admin_in.[user_id]
        LEFT JOIN {`schema`}.[users] admin_out
          ON a.[revoked_by] = admin_out.[user_id]
        WHERE a.[app_id] = {aid}
        ORDER BY COALESCE(a.[revoked_at], a.[assigned_at]) DESC;
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Grant or reactivate a system role assignment (Upsert)
#'
#' Executes a SQL `MERGE` against `[user_role_assignments]`. If a matching
#' `(user_id, role_id, app_id)` row exists, it is reactivated and cleared of
#' revocation metadata; otherwise a new active assignment row is inserted.
#'
#' @details
#' Modifies `[user_role_assignments]` within
#' `utils_resolve_schema("db_schema_01sr")`. Supports passing an existing
#' `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts or updates a row in `[user_role_assignments]`
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Target user ID receiving the role.
#' @param role_id Integer scalar. ID of the role being assigned.
#' @param app_id Integer scalar. Target application ID scope.
#' @param assigned_by Integer scalar. User ID of the administrator granting access.
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` on
#'   success, `0L` on validation failure or database error).
#'
#' @family roles database functions
#' @seealso [db_roles_update_user_role()], [db_roles_revoke_assignment()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_roles_insert_assignment(user_id = 42L, role_id = 2L, app_id = 1L, assigned_by = 1L)
#' }
db_roles_insert_assignment <- function(
  user_id,
  role_id,
  app_id,
  assigned_by,
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  rid <- suppressWarnings(as.integer(role_id))
  aid <- suppressWarnings(as.integer(app_id))
  aby <- suppressWarnings(as.integer(assigned_by))

  if (
    any(vapply(
      list(uid, rid, aid, aby),
      function(x) length(x) != 1L || is.na(x),
      logical(1)
    ))
  ) {
    log_event(paste0(fn_name, " failed: invalid integer parameters"), type = 2)
    return(0L)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        MERGE {`schema`}.[user_role_assignments] AS target
        USING (
          SELECT
            {uid} AS [user_id],
            {rid} AS [role_id],
            {aid} AS [app_id]
        ) AS source
        ON (
          target.[user_id] = source.[user_id]
          AND target.[role_id] = source.[role_id]
          AND target.[app_id] = source.[app_id]
        )
        WHEN MATCHED THEN
          UPDATE SET
            [is_active] = 1,
            [assigned_at] = SYSUTCDATETIME(),
            [assigned_by] = {aby},
            [revoked_at] = NULL,
            [revoked_by] = NULL
        WHEN NOT MATCHED THEN
          INSERT ([user_id], [role_id], [app_id], [is_active], [assigned_at], [assigned_by])
          VALUES (source.[user_id], source.[role_id], source.[app_id], 1, SYSUTCDATETIME(), {aby});
        ",
        .con = conn
      )

      as.integer(utils_db_execute(conn, query))
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      0L
    }
  )
}

#' Replace a user's active application role atomically
#'
#' Deactivates all existing active role assignments for `(user_id, app_id)`
#' and inserts a new active assignment row inside a single SQL transaction.
#'
#' @details
#' Executes a `BEGIN TRANSACTION ... COMMIT` batch against
#' `[user_role_assignments]` within `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates existing active rows and inserts a new row in `[user_role_assignments]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Target user ID.
#' @param role_id Integer scalar. New role ID to assign.
#' @param app_id Integer scalar. Target application ID scope.
#' @param assigned_by Integer scalar. Administrator user ID making the change.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` for the
#'   inserted row on success, `0L` on validation failure or database error).
#'
#' @family roles database functions
#' @seealso [db_roles_insert_assignment()], [db_roles_revoke_assignment()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_roles_update_user_role(user_id = 42L, role_id = 3L, app_id = 1L, assigned_by = 1L)
#' }
db_roles_update_user_role <- function(
  user_id,
  role_id,
  app_id,
  assigned_by,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  rid <- suppressWarnings(as.integer(role_id))
  aid <- suppressWarnings(as.integer(app_id))
  aby <- suppressWarnings(as.integer(assigned_by))

  if (
    any(vapply(
      list(uid, rid, aid, aby),
      function(x) length(x) != 1L || is.na(x),
      logical(1)
    ))
  ) {
    log_event(paste0(fn_name, " failed: invalid integer parameters"), type = 2)
    return(0L)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        BEGIN TRANSACTION;

        UPDATE {`schema`}.[user_role_assignments]
        SET
          [is_active] = 0,
          [revoked_at] = SYSUTCDATETIME(),
          [revoked_by] = {aby}
        WHERE [user_id] = {uid}
          AND [app_id] = {aid}
          AND [is_active] = 1;

        INSERT INTO {`schema`}.[user_role_assignments]
          ([user_id], [role_id], [app_id], [is_active], [assigned_by], [assigned_at])
        VALUES
          ({uid}, {rid}, {aid}, 1, {aby}, SYSUTCDATETIME());

        COMMIT;
        ",
        .con = conn
      )

      as.integer(utils_db_execute(conn, query))
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      0L
    }
  )
}

#' Revoke a system role assignment (Soft-Delete)
#'
#' Marks an active role assignment as inactive (`is_active = 0`) and records the
#' UTC revocation timestamp and administrator user ID without deleting the row.
#'
#' @details
#' Updates `[user_role_assignments]` within
#' `utils_resolve_schema("db_schema_01sr")`. Supports passing an existing
#' `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates `[user_role_assignments]` in the database
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Target user ID losing the role.
#' @param role_id Integer scalar. ID of the role being revoked.
#' @param app_id Integer scalar. Target application ID scope.
#' @param revoked_by Integer scalar. User ID of the administrator revoking access.
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` on
#'   success, `0L` if no active assignment matched, inputs were invalid, or an
#'   error occurred).
#'
#' @family roles database functions
#' @seealso [db_roles_insert_assignment()], [db_roles_get_assignment_history()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_roles_revoke_assignment(user_id = 42L, role_id = 2L, app_id = 1L, revoked_by = 1L)
#' }
db_roles_revoke_assignment <- function(
  user_id,
  role_id,
  app_id,
  revoked_by,
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  rid <- suppressWarnings(as.integer(role_id))
  aid <- suppressWarnings(as.integer(app_id))
  rby <- suppressWarnings(as.integer(revoked_by))

  if (
    any(vapply(
      list(uid, rid, aid, rby),
      function(x) length(x) != 1L || is.na(x),
      logical(1)
    ))
  ) {
    log_event(paste0(fn_name, " failed: invalid integer parameters"), type = 2)
    return(0L)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        UPDATE {`schema`}.[user_role_assignments]
        SET
          [is_active] = 0,
          [revoked_at] = SYSUTCDATETIME(),
          [revoked_by] = {rby}
        WHERE [user_id] = {uid}
          AND [role_id] = {rid}
          AND [app_id] = {aid}
          AND [is_active] = 1;
        ",
        .con = conn
      )

      as.integer(utils_db_execute(conn, query))
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      0L
    }
  )
}
