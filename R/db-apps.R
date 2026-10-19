# ==============================================================================
# Centralized Admin & App Directory Database Helpers
# ==============================================================================

#' Retrieve All Registered Apps
#' @export
db_get_all_apps <- function() {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT 
        [app_id], [app_name], [app_url], [app_active], 
        [version], [team], [division], [update_cycle], 
        [linking_table], [linking_column]
     FROM {utils_resolve_schema('db_schema_01aidt')}.[app_list]
     ORDER BY [app_name];",
    .con = conn
  )
  utils_db_get_query(conn, query)
}

#' Upsert Application Details
#' @export
db_save_app <- function(
  app_id = NULL,
  app_name,
  app_url,
  app_active,
  version,
  team,
  division,
  update_cycle,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  if (is.null(app_id) || is.na(app_id) || identical(as.character(app_id), "")) {
    query <- glue::glue_sql(
      "INSERT INTO {utils_resolve_schema('db_schema_01aidt')}.[app_list]
         ([app_name], [app_url], [app_active], [version], [team], [division], [update_cycle], [created_by], [created_date])
       VALUES 
         ({app_name}, {app_url}, {as.integer(app_active)}, {version}, {team}, {division}, {update_cycle}, {as.integer(user_id)}, SYSUTCDATETIME());",
      .con = conn
    )
  } else {
    query <- glue::glue_sql(
      "UPDATE {utils_resolve_schema('db_schema_01aidt')}.[app_list]
       SET [app_name] = {app_name},
           [app_url] = {app_url},
           [app_active] = {as.integer(app_active)},
           [version] = {version},
           [team] = {team},
           [division] = {division},
           [update_cycle] = {update_cycle},
           [modified_by] = {as.integer(user_id)},
           [modified_date] = SYSUTCDATETIME()
       WHERE [app_id] = {as.integer(app_id)};",
      .con = conn
    )
  }
  utils_db_execute(conn, query)
}

#' Retrieve Maintainers for an App
#' @export
db_get_app_maintainers <- function(app_id) {
  shiny::req(app_id)
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT 
        m.[id], m.[app_id], m.[user_id], m.[is_primary],
        u.[username], u.[email]
     FROM {utils_resolve_schema('db_schema_01sr')}.[app_maintainers] m
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[users] u ON m.[user_id] = u.[user_id]
     WHERE m.[app_id] = {as.integer(app_id)};",
    .con = conn
  )
  utils_db_get_query(conn, query)
}

#' Add App Maintainer
#' @export
db_add_app_maintainer <- function(
  app_id,
  user_id,
  is_primary = 1,
  created_by = 1
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[app_maintainers]
       ([app_id], [user_id], [is_primary], [created_by], [created_date])
     VALUES 
       ({as.integer(app_id)}, {as.integer(user_id)}, {as.integer(is_primary)}, {as.integer(created_by)}, SYSUTCDATETIME());",
    .con = conn
  )
  utils_db_execute(conn, query)
}

#' Remove App Maintainer
#' @export
db_delete_app_maintainer <- function(maintainer_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "DELETE FROM {utils_resolve_schema('db_schema_01sr')}.[app_maintainers]
     WHERE [id] = {as.integer(maintainer_id)};",
    .con = conn
  )
  utils_db_execute(conn, query)
}

#' Retrieve All Portal Messages
#' @export
db_get_portal_messages <- function(app_id = NULL) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  clause <- if (!is.null(app_id)) {
    glue::glue_sql("WHERE m.[app_id] = {as.integer(app_id)}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue::glue_sql(
    "SELECT 
        m.[message_id], m.[message_date], m.[message_text], 
        m.[app_id], m.[priority], m.[is_active],
        COALESCE(a.[app_name], 'Global / All Apps') AS [app_name],
        u.[username] AS [posted_by]
     FROM {utils_resolve_schema('db_schema_01sr')}.[portal_messages] m
     LEFT JOIN {utils_resolve_schema('db_schema_01aidt')}.[app_list] a ON m.[app_id] = a.[app_id]
     LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[users] u ON m.[user_id] = u.[user_id]
     {clause}
     ORDER BY m.[created_on] DESC;",
    .con = conn
  )
  utils_db_get_query(conn, query)
}

#' Upsert Portal Message
#' @export
db_save_portal_message <- function(
  message_id = NULL,
  app_id,
  message_text,
  priority = 2,
  is_active = 1,
  user_id = 1
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  if (
    is.null(message_id) ||
      is.na(message_id) ||
      identical(as.character(message_id), "")
  ) {
    query <- glue::glue_sql(
      "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
         ([app_id], [message_text], [priority], [is_active], [user_id], [message_date], [created_on])
       VALUES 
         ({as.integer(app_id)}, {message_text}, {as.integer(priority)}, {as.integer(is_active)}, {as.integer(user_id)}, SYSUTCDATETIME(), SYSUTCDATETIME());",
      .con = conn
    )
  } else {
    query <- glue::glue_sql(
      "UPDATE {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
       SET [app_id] = {as.integer(app_id)},
           [message_text] = {message_text},
           [priority] = {as.integer(priority)},
           [is_active] = {as.integer(is_active)}
       WHERE [message_id] = {as.integer(message_id)};",
      .con = conn
    )
  }
  utils_db_execute(conn, query)
}

#' Delete Portal Message
#' @export
db_delete_portal_message <- function(message_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "DELETE FROM {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
     WHERE [message_id] = {as.integer(message_id)};",
    .con = conn
  )
  utils_db_execute(conn, query)
}
