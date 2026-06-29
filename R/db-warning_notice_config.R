#' Retrieve Notice Status Configurations
#'
#' Returns all lookup records from the `twn_status_config` table containing
#' status names and descriptions.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] with the following columns:
#' \describe{
#'   \item{twn_status_id}{Integer status identifier.}
#'   \item{twn_status_name}{Character name of the status.}
#'   \item{twn_status_desc}{Character description of what the status represents.}
#'   \item{created_by}{Character user who created the record.}
#'   \item{created_on}{POSIXct date-time of creation.}
#'   \item{updated_by}{Character user who last updated the record.}
#'   \item{updated_on}{POSIXct date-time of last update.}
#' }
#'
#' @export
db_get_status_config <- function() {
  log_event("Starting db_get_status_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_status_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT 
        [twn_status_id]
       ,[twn_status_name]
       ,[twn_status_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
    FROM {schema}.[twn_status_config]
    ORDER BY [twn_status_id];
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}


#' Insert a New Status Configuration
#'
#' Adds a new status type to the lookup configuration table.
#'
#' @param status_data A named list containing `twn_status_name`, `twn_status_desc`,
#' and `created_by`.
#'
#' @return The result of the execution.
#'
#' @export
db_insert_status_config <- function(status_data) {
  log_event("Starting db_insert_status_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_status_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_status_config] (
        [twn_status_name]
       ,[twn_status_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
    ) VALUES (
        {status_data$twn_status_name}
       ,{status_data$twn_status_desc}
       ,{status_data$created_by}
       ,GETDATE()
       ,{status_data$created_by}
       ,GETDATE()
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Status Configuration
#'
#' Updates the details of a specific status configuration record.
#'
#' @param status_id Integer. The `twn_status_id` of the record to modify.
#' @param update_data A named list containing `twn_status_name`, `twn_status_desc`,
#' and `updated_by`.
#'
#' @return The result of the execution.
#'
#' @export
db_update_status_config <- function(status_id, update_data) {
  log_event(paste("Starting db_update_status_config for ID:", status_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_status_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_status_config]
    SET [twn_status_name] = {update_data$twn_status_name}
       ,[twn_status_desc] = {update_data$twn_status_desc}
       ,[updated_by]      = {update_data$updated_by}
       ,[updated_on]      = GETDATE()
    WHERE [twn_status_id] = {status_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Retrieve Notice Type Configurations
#'
#' Returns all lookup records from the `twn_type_config` table containing
#' notice types, descriptions, and web section routing metadata.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] with the following columns:
#' \describe{
#'   \item{twn_type_id}{Integer type identifier.}
#'   \item{twn_type_name}{Character name of the notice type.}
#'   \item{twn_type_desc}{Character description of the notice type.}
#'   \item{created_by}{Character user who created the record.}
#'   \item{created_on}{POSIXct date-time of creation.}
#'   \item{updated_by}{Character user who last updated the record.}
#'   \item{updated_on}{POSIXct date-time of last update.}
#'   \item{coc}{Character or logical compliance check flag.}
#'   \item{web_section}{Character section mapping for web display.}
#' }
#'
#' @export
db_get_type_config <- function() {
  log_event("Starting db_get_type_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_type_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT 
        [twn_type_id]
       ,[twn_type_name]
       ,[twn_type_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
       ,[coc]
       ,[web_section]
    FROM {schema}.[twn_type_config]
    ORDER BY [twn_type_id];
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}

#' Insert a New Notice Type Configuration
#'
#' Adds a new notice type to the lookup configuration table.
#'
#' @param type_data A named list containing `twn_type_name`, `twn_type_desc`,
#' `created_by`, `coc`, and `web_section`.
#'
#' @return The result of the execution.
#'
#' @export
db_insert_type_config <- function(type_data) {
  log_event("Starting db_insert_type_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_type_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_type_config] (
        [twn_type_name]
       ,[twn_type_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
       ,[coc]
       ,[web_section]
    ) VALUES (
        {type_data$twn_type_name}
       ,{type_data$twn_type_desc}
       ,{type_data$created_by}
       ,GETDATE()
       ,{type_data$created_by}
       ,GETDATE()
       ,{type_data$coc}
       ,{type_data$web_section}
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Notice Type Configuration
#'
#' Updates the details of a specific notice type configuration record.
#'
#' @param type_id Integer. The `twn_type_id` of the record to modify.
#' @param update_data A named list containing `twn_type_name`, `twn_type_desc`,
#' `updated_by`, `coc`, and `web_section`.
#'
#' @return The result of the execution.
#'
#' @export
db_update_type_config <- function(type_id, update_data) {
  log_event(paste("Starting db_update_type_config for ID:", type_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_type_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_type_config]
    SET [twn_type_name] = {update_data$twn_type_name}
       ,[twn_type_desc] = {update_data$twn_type_desc}
       ,[updated_by]    = {update_data$updated_by}
       ,[updated_on]    = GETDATE()
       ,[coc]           = {update_data$coc}
       ,[web_section]   = {update_data$web_section}
    WHERE [twn_type_id] = {type_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}
