#' Query Master Users Directory by Token Variants
#'
#' Executes a direct lookup against the master users ledger using clean username or email strings.
#'
#' @param conn A DBI database connection object.
#' @param token Character scalar. The cleaned, lowercase username or email address.
#' @return A data frame containing matching user records, or an empty data frame if none found.
#' @export
db_get_user_by_token <- function(conn, token) {
  log_event("Starting db_get_user_by_token")
  on.exit(log_event("Finished db_get_user_by_token"), add = TRUE)

  shiny::req(conn)

  query <- glue::glue_sql(
    "SELECT [user_id], [username], [email] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] 
     WHERE LOWER([username]) = {token} OR LOWER([email]) = {token};",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_user_by_token failed: ", e$message)
      data.frame()
    }
  )
}

#' Query User Email Aliases Ledger
#'
#' Looks up a corporate shorthand username alias to retrieve its canonical target email address.
#'
#' @param conn A DBI database connection object.
#' @param token Character scalar. The cleaned shorthand network username alias (e.g., 'bsmith7').
#' @return A character string containing the resolved email address, or NULL if no alias exists.
#' @export
db_get_user_email_alias <- function(conn, token) {
  log_event("Starting db_get_user_email_alias")
  on.exit(log_event("Finished db_get_user_email_alias"), add = TRUE)

  shiny::req(conn)

  query <- glue::glue_sql(
    "SELECT [email_address] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_identity_aliases] 
     WHERE LOWER([username_alias]) = {token};",
    .con = conn
  )

  res <- tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_user_email_alias failed: ", e$message)
      NULL
    }
  )

  if (!is.null(res) && nrow(res) > 0) {
    return(tolower(res$email_address[1]))
  }
  return(NULL)
}

#' Query Master Users Directory Strictly by Email
#'
#' @param conn A DBI database connection object.
#' @param email Character scalar. The canonical email address to check.
#' @return A data frame containing matching user records, or an empty data frame if none found.
#' @export
db_get_user_by_email <- function(conn, email) {
  log_event("Starting db_get_user_by_email")
  on.exit(log_event("Finished db_get_user_by_email"), add = TRUE)

  shiny::req(conn)

  query <- glue::glue_sql(
    "SELECT [user_id], [username], [email] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] 
     WHERE LOWER([email]) = {email};",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_user_by_email failed: ", e$message)
      data.frame()
    }
  )
}

#' Provision a New User Record (Just-In-Time)
#'
#' Injects a pristine, default record into the users directory and returns the newly generated user ID.
#'
#' @param conn A DBI database connection object.
#' @param username Character scalar. The short name prefix for the user.
#' @param email Character scalar. The unique canonical email identity.
#' @return Integer scalar. The newly inserted [user_id], or 1L if insertion fails.
#' @export
db_insert_new_user <- function(conn, username, email) {
  log_event("Starting db_insert_new_user")
  on.exit(log_event("Finished db_insert_new_user"), add = TRUE)

  shiny::req(conn, email)

  insert_query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[users] ([username], [email], [created_at])
     VALUES ({username}, {email}, SYSUTCDATETIME());",
    .con = conn
  )

  tryCatch(
    {
      utils_db_execute(conn, insert_query)

      # Re-query to capture the identity value securely
      id_res <- db_get_user_by_email(conn, email)
      if (nrow(id_res) > 0) {
        return(as.integer(id_res$user_id[1]))
      }
      return(1L)
    },
    error = function(e) {
      warning("db_insert_new_user failed: ", e$message)
      return(1L)
    }
  )
}

#' Query Active User Privileges and Roles
#'
#' Retrieves the system role records bound to a given user id within a specific app context.
#'
#' @param conn A DBI database connection object.
#' @param user_id Integer scalar. The resolved internal identity tracking key.
#' @param app_id Integer scalar. The specific app scope key context.
#' @return A character vector of active system role names (e.g., 'admin'), or NULL.
#' @export
db_get_user_roles_by_app <- function(conn, user_id, app_id) {
  log_event("Starting db_get_user_roles_by_app")
  on.exit(log_event("Finished db_get_user_roles_by_app"), add = TRUE)

  shiny::req(conn, user_id, app_id)

  query <- glue::glue_sql(
    "SELECT r.[role_name] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] r ON a.[role_id] = r.[role_id]
     WHERE a.[user_id] = {as.integer(user_id)} 
       AND a.[app_id] = {as.integer(app_id)} 
       AND a.[is_active] = 1;",
    .con = conn
  )

  res <- tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_user_roles_by_app failed: ", e$message)
      NULL
    }
  )

  if (!is.null(res) && nrow(res) > 0) {
    return(as.character(res$role_name))
  }
  return(NULL)
}
