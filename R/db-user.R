# ==============================================================================
# File: R/db-users.R
# Purpose: Database query and mutation functions for user directory & identity
#
# Functions:
#   - db_users_get_all()            : Fetch all master user directory records
#   - db_users_get_by_id()          : Fetch user record by primary key user_id
#   - db_users_get_by_email()       : Fetch user record by canonical email
#   - db_users_get_by_username()    : Fetch user record by network username
#   - db_users_get_email_alias()    : Resolve shorthand username alias to email
#   - db_users_insert()             : Provision a new user record (JIT)
#   - db_users_update_email()       : Update canonical email for a user_id
#   - db_users_update_last_login()  : Stamp last_login with current UTC time
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all master user directory records
#'
#' Retrieves all user profiles from `[users]`, ordered alphabetically by
#' `username`.
#'
#' @details
#' Queries `[users]` within the schema resolved by
#' `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 5 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Network/AD username.}
#'     \item{`email`}{Character. Canonical corporate email address.}
#'     \item{`created_at`}{POSIXct. UTC timestamp when the record was created.}
#'     \item{`last_login`}{POSIXct. UTC timestamp of the user's most recent login.}
#'   }
#'
#' @family users database functions
#' @seealso [db_users_get_by_id()], [db_users_get_by_email()],
#'   [db_users_get_by_username()], [sql_manager()], [utils_resolve_schema()]
#'
#' @export
#' @examples
#' \dontrun{
#' users <- db_users_get_all()
#' }
db_users_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          [user_id],
          [username],
          [email],
          [created_at],
          [last_login]
        FROM {`schema`}.[users]
        ORDER BY [username];
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

#' Fetch a user record by primary key ID
#'
#' Retrieves a single user profile from `[users]` matching `user_id`.
#'
#' @details
#' Queries `[users]` within `utils_resolve_schema("db_schema_01sr")`. Returns
#' `NULL` immediately if `user_id` cannot be coerced to a valid non-`NA` integer.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Primary key user identifier.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (0 or 1 row) with 3 columns (`user_id`, `username`,
#'   `email`), or `NULL` on invalid input or database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Network/AD username.}
#'     \item{`email`}{Character. Canonical corporate email address.}
#'   }
#'
#' @family users database functions
#' @seealso [db_users_get_all()], [db_users_get_by_email()],
#'   [db_users_get_by_username()]
#'
#' @export
#' @examples
#' \dontrun{
#' user <- db_users_get_by_id(42L)
#' }
db_users_get_by_id <- function(user_id, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  if (length(uid) != 1L || is.na(uid)) {
    log_event(paste0(fn_name, " failed: invalid user_id"), type = 2)
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
          [user_id],
          [username],
          [email]
        FROM {`schema`}.[users]
        WHERE [user_id] = {uid};
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

#' Fetch a user record by email address
#'
#' Performs a case-insensitive, whitespace-trimmed lookup against `[users].[email]`.
#'
#' @details
#' Queries `[users]` within `utils_resolve_schema("db_schema_01sr")` using
#' parameterised `glue::glue_sql()`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param email Character scalar. The canonical email address to look up.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (0 or 1 row) with 3 columns (`user_id`, `username`,
#'   `email`), or `NULL` on invalid input or database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Network/AD username.}
#'     \item{`email`}{Character. Canonical corporate email address.}
#'   }
#'
#' @family users database functions
#' @seealso [db_users_get_by_username()], [db_users_get_email_alias()],
#'   [db_users_insert()]
#'
#' @export
#' @examples
#' \dontrun{
#' user <- db_users_get_by_email("ben.smith@education.gov.uk")
#' }
db_users_get_by_email <- function(email, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  if (!is.character(email) || length(email) != 1L || !nzchar(trimws(email))) {
    log_event(paste0(fn_name, " failed: invalid email"), type = 2)
    return(NULL)
  }

  clean_email <- tolower(trimws(email))
  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          [user_id],
          [username],
          [email]
        FROM {`schema`}.[users]
        WHERE LOWER([email]) = {clean_email};
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

#' Fetch a user record by network username
#'
#' Performs a case-insensitive, whitespace-trimmed lookup against
#' `[users].[username]`.
#'
#' @details
#' Queries `[users]` within `utils_resolve_schema("db_schema_01sr")` using
#' parameterised `glue::glue_sql()`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param username Character scalar. Legacy AD/network username token (e.g., `"bsmith7"`).
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (0 or 1 row) with 3 columns (`user_id`, `username`,
#'   `email`), or `NULL` on invalid input or database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Primary key user identifier.}
#'     \item{`username`}{Character. Network/AD username.}
#'     \item{`email`}{Character. Canonical corporate email address.}
#'   }
#'
#' @family users database functions
#' @seealso [db_users_get_by_email()], [db_users_get_email_alias()]
#'
#' @export
#' @examples
#' \dontrun{
#' user <- db_users_get_by_username("bsmith7")
#' }
db_users_get_by_username <- function(username, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  if (
    !is.character(username) ||
      length(username) != 1L ||
      !nzchar(trimws(username))
  ) {
    log_event(paste0(fn_name, " failed: invalid username"), type = 2)
    return(NULL)
  }

  clean_username <- tolower(trimws(username))
  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          [user_id],
          [username],
          [email]
        FROM {`schema`}.[users]
        WHERE LOWER([username]) = {clean_username};
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

#' Resolve a shorthand username alias to a canonical email address
#'
#' Queries `[user_identity_aliases]` to map a corporate shorthand username
#' alias (e.g., `"bsmith7"`) to its canonical email address.
#'
#' @details
#' Queries `[user_identity_aliases]` within
#' `utils_resolve_schema("db_schema_01sr")` using case-insensitive matching.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param token Character scalar. Shorthand network username alias.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Character scalar containing the lowercased canonical email address,
#'   or `NULL` if no matching alias exists or an error occurs.
#'
#' @family users database functions
#' @seealso [db_users_get_by_email()], [db_users_get_by_username()]
#'
#' @export
#' @examples
#' \dontrun{
#' email <- db_users_get_email_alias("bsmith7")
#' }
db_users_get_email_alias <- function(token, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  if (!is.character(token) || length(token) != 1L || !nzchar(trimws(token))) {
    log_event(paste0(fn_name, " failed: invalid token"), type = 2)
    return(NULL)
  }

  clean_token <- tolower(trimws(token))
  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT [email_address]
        FROM {`schema`}.[user_identity_aliases]
        WHERE LOWER([username_alias]) = {clean_token};
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, query)
      if (!is.null(res) && nrow(res) > 0L) {
        return(tolower(trimws(res$email_address[1L])))
      }
      NULL
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Provision a new user record (Just-In-Time)
#'
#' Inserts a new user into `[users]` with a UTC `created_at` timestamp and
#' atomically returns the newly generated `user_id` via SQL `OUTPUT INSERTED`.
#'
#' @details
#' Inserts into `[users]` within `utils_resolve_schema("db_schema_01sr")`.
#' Email addresses are lowercased and trimmed before insertion.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a row into `[users]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param username Character scalar. The short name or token for the user.
#' @param email Character scalar. The unique canonical email address.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Integer scalar containing the newly inserted `user_id`, or `NULL`
#'   if validation fails or a database error occurs.
#'
#' @family users database functions
#' @seealso [db_users_get_by_email()], [db_users_update_email()]
#'
#' @export
#' @examples
#' \dontrun{
#' new_id <- db_users_insert("bsmith7", "ben.smith@education.gov.uk")
#' }
db_users_insert <- function(username, email, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  if (!is.character(email) || length(email) != 1L || !nzchar(trimws(email))) {
    log_event(paste0(fn_name, " failed: invalid email"), type = 2)
    return(NULL)
  }

  clean_user <- if (!is.null(username) && nzchar(trimws(username))) {
    trimws(as.character(username))
  } else {
    NA_character_
  }
  clean_email <- tolower(trimws(email))
  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[users] ([username], [email], [created_at])
        OUTPUT INSERTED.[user_id]
        VALUES ({clean_user}, {clean_email}, SYSUTCDATETIME());
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, query)
      if (!is.null(res) && nrow(res) > 0L) {
        return(as.integer(res$user_id[1L]))
      }
      NULL
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Associate a canonical email address with an existing user record
#'
#' Updates `[users].[email]` for a given `user_id` with a lowercased,
#' whitespace-trimmed canonical email address.
#'
#' @details
#' Updates `[users]` within `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates `[users]` in the database
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Primary key user identifier.
#' @param email Character scalar. Canonical email address.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` on
#'   success, `0L` if no matching row or an error occurred).
#'
#' @family users database functions
#' @seealso [db_users_get_by_id()], [db_users_insert()]
#'
#' @export
#' @examples
#' \dontrun{
#' rows <- db_users_update_email(42L, "ben.smith@education.gov.uk")
#' }
db_users_update_email <- function(user_id, email, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  if (
    length(uid) != 1L ||
      is.na(uid) ||
      !is.character(email) ||
      length(email) != 1L ||
      !nzchar(trimws(email))
  ) {
    log_event(paste0(fn_name, " failed: invalid user_id or email"), type = 2)
    return(0L)
  }

  clean_email <- tolower(trimws(email))
  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        UPDATE {`schema`}.[users]
        SET [email] = {clean_email}
        WHERE [user_id] = {uid};
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

#' Update user last login timestamp
#'
#' Sets `[users].[last_login]` to the current UTC timestamp (`SYSUTCDATETIME()`)
#' for a specific `user_id`.
#'
#' @details
#' Updates `[users]` within `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates `[users].[last_login]` in the database
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Canonical primary key user identifier.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Logical scalar (invisibly). `TRUE` if a user row was updated,
#'   `FALSE` if `user_id` was invalid, not found, or an error occurred.
#'
#' @family users database functions
#' @seealso [db_users_get_by_id()], [db_users_insert()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_users_update_last_login(42L)
#' }
db_users_update_last_login <- function(user_id, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  if (length(uid) != 1L || is.na(uid)) {
    log_event(paste0(fn_name, " failed: invalid user_id"), type = 2)
    return(invisible(FALSE))
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        UPDATE {`schema`}.[users]
        SET [last_login] = SYSUTCDATETIME()
        WHERE [user_id] = {uid};
        ",
        .con = conn
      )

      rows_affected <- utils_db_execute(conn, query)
      invisible(rows_affected > 0L)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      invisible(FALSE)
    }
  )
}
