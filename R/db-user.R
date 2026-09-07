#' Query Master Users Directory Strictly by Email
#'
#' @param email Character scalar. The canonical email address to check.
#' @return A data frame containing matching user records, or an empty data frame if none found.
#' @export
db_get_user_by_email <- function(email) {
  log_event("Starting db_get_user_by_email")
  on.exit(log_event("Finished db_get_user_by_email"), add = TRUE)

  shiny::req(email)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [user_id], [username], [email] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] 
     WHERE LOWER([email]) = {tolower(trimws(email))};",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_user_by_email error: ", e$message))
      warning("db_get_user_by_email failed: ", e$message)
      data.frame()
    }
  )
}

#' Query Master Users Directory Strictly by Username
#'
#' @param username Character scalar. Legacy AD username token.
#' @return A data frame containing matching user records, or an empty data frame if none found.
#' @export
db_get_user_by_username <- function(username) {
  log_event("Starting db_get_user_by_username")
  on.exit(log_event("Finished db_get_user_by_username"), add = TRUE)

  shiny::req(username)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [user_id], [username], [email] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] 
     WHERE LOWER([username]) = {tolower(trimws(username))};",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_user_by_username error: ", e$message))
      warning("db_get_user_by_username failed: ", e$message)
      data.frame()
    }
  )
}

#' Query User Email Aliases Ledger
#'
#' Looks up a corporate shorthand username alias to retrieve its canonical target email address.
#'
#' @param token Character scalar. Shorthand network username alias (e.g., 'bsmith7').
#' @return A character string containing the resolved email address, or NULL if no alias exists.
#' @export
db_get_user_email_alias <- function(token) {
  log_event("Starting db_get_user_email_alias")
  on.exit(log_event("Finished db_get_user_email_alias"), add = TRUE)

  shiny::req(token)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [email_address] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_identity_aliases] 
     WHERE LOWER([username_alias]) = {tolower(trimws(token))};",
    .con = conn
  )

  res <- tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_user_email_alias error: ", e$message))
      warning("db_get_user_email_alias failed: ", e$message)
      NULL
    }
  )

  if (!is.null(res) && nrow(res) > 0) {
    return(tolower(res$email_address[1]))
  }
  NULL
}

#' Associate Email Address with Existing User Record
#'
#' Updates an existing legacy user record with their verified corporate email.
#'
#' @param user_id Integer scalar. Primary key user identifier.
#' @param email Character scalar. Canonical email address.
#' @return Integer scalar. Rows affected (1 on success).
#' @export
db_update_user_email <- function(user_id, email) {
  log_event(paste0("Starting db_update_user_email for user_id: ", user_id))
  on.exit(log_event("Finished db_update_user_email"), add = TRUE)

  shiny::req(user_id, email)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "UPDATE {utils_resolve_schema('db_schema_01sr')}.[users]
     SET [email] = {tolower(trimws(email))}
     WHERE [user_id] = {as.integer(user_id)};",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      log_event(paste0("db_update_user_email error: ", e$message))
      warning("db_update_user_email failed: ", e$message)
      0L
    }
  )
}

#' Provision a New User Record (Just-In-Time)
#'
#' Inserts a new user record into the users directory and returns the generated user ID atomically.
#'
#' @param username Character scalar. The short name or token for the user.
#' @param email Character scalar. The unique canonical email address.
#' @return Integer scalar. The newly inserted user_id, or 1L if insertion fails.
#' @export
db_insert_new_user <- function(username, email) {
  log_event("Starting db_insert_new_user")
  on.exit(log_event("Finished db_insert_new_user"), add = TRUE)

  shiny::req(email)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[users] ([username], [email], [created_at])
     OUTPUT INSERTED.[user_id]
     VALUES ({username}, {tolower(trimws(email))}, SYSUTCDATETIME());",
    .con = conn
  )

  tryCatch(
    {
      res <- utils_db_get_query(conn, query)
      if (!is.null(res) && nrow(res) > 0) {
        return(as.integer(res$user_id[1]))
      }
      1L
    },
    error = function(e) {
      log_event(paste0("db_insert_new_user error: ", e$message))
      warning("db_insert_new_user failed: ", e$message)
      1L
    }
  )
}

#' Query User Record by Primary Key ID
#'
#' @param user_id Integer scalar. Primary key identifier.
#' @return Data frame containing user details or empty data frame.
#' @export
db_get_user_by_id <- function(user_id) {
  log_event("Starting db_get_user_by_id")
  on.exit(log_event("Finished db_get_user_by_id"), add = TRUE)

  shiny::req(user_id)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [user_id], [username], [email] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] 
     WHERE [user_id] = {as.integer(user_id)};",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_user_by_id error: ", e$message))
      data.frame()
    }
  )
}

#' Update User Last Login Timestamp
#'
#' Sets the last_login column to the current UTC timestamp for a specific user_id.
#'
#' @param user_id Integer canonical user_id.
#' @return Logical indicating whether a row was affected.
#' @export
db_update_user_last_login <- function(user_id) {
  log_event(glue::glue(
    "Starting db_update_user_last_login for user_id: {user_id}"
  ))

  uid <- suppressWarnings(as.integer(user_id))
  if (is.na(uid)) {
    log_event("Failed db_update_user_last_login: invalid user_id")
    return(FALSE)
  }

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_user_last_login")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01sr')}.[users]
    SET [last_login] = SYSUTCDATETIME()
    WHERE [user_id] = {uid};
    ",
    .con = conn
  )

  rows_affected <- tryCatch(
    DBI::dbExecute(conn, query),
    error = function(e) {
      log_event(glue::glue("Error updating last_login: {e$message}"))
      0L
    }
  )

  invisible(rows_affected > 0)
}
