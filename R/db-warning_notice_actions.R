# ==============================================================================
# SECTION 1: DATE CONFIG / TYPES (Read, Add, Edit)
# ==============================================================================

#' Retrieve Date Type Configurations
#'
#' Returns all configured milestone/date types from the `twn_date_config` table.
#'
#' @export
db_get_date_config <- function() {
  log_event("Starting db_get_date_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_date_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT 
        [twn_date_id]
       ,[twn_date_name]
       ,[twn_date_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
    FROM {schema}.[twn_date_config]
    ORDER BY [twn_date_id];
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}

#' Insert a New Date Type Configuration
#'
#' Adds a new milestone type/category to the date configuration table.
#'
#' @param config_data A named list containing `twn_date_name`, `twn_date_desc`, and `created_by`.
#'
#' @export
db_insert_date_config <- function(config_data) {
  log_event("Starting db_insert_date_config")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_date_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_date_config] (
        [twn_date_name]
       ,[twn_date_desc]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
    ) VALUES (
        {config_data$twn_date_name}
       ,{config_data$twn_date_desc}
       ,{config_data$created_by}
       ,GETDATE()
       ,{config_data$created_by}
       ,GETDATE()
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Date Type Configuration
#'
#' Modifies meta-details for a specific date type configuration record.
#'
#' @param date_config_id Integer. The ID of the configuration record to update.
#' @param update_data A named list containing `twn_date_name`, `twn_date_desc`, and `updated_by`.
#'
#' @export
db_update_date_config <- function(date_config_id, update_data) {
  log_event(paste("Starting db_update_date_config for ID:", date_config_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_date_config")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_date_config]
    SET [twn_date_name] = {update_data$twn_date_name}
       ,[twn_date_desc] = {update_data$twn_date_desc}
       ,[updated_by]    = {update_data$updated_by}
       ,[updated_on]    = GETDATE()
    WHERE [twn_date_id] = {date_config_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}


# ==============================================================================
# SECTION 2: DATE TRACKING / ACTIONS (Read, Add, Edit, Remove)
# ==============================================================================

#' Retrieve Date Tracking Records for a Notice
#'
#' Returns all active tracking dates and actions linked to a specific Notice ID (`twn_id`),
#' matching against configurations to pull in type names.
#'
#' @param twn_id Integer. The notice identifier to filter actions by.
#'
#' @export
db_get_notice_actions <- function(twn_id) {
  log_event(paste("Starting db_get_notice_actions for TWN ID:", twn_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_notice_actions")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT 
        t.[twn_date_id]
       ,t.[twn_id]
       ,t.[twn_date_type_id]
       ,c.[twn_date_name] AS [date_type_name]
       ,t.[twn_date]
       ,t.[twn_date_notes]
       ,t.[created_by]
       ,t.[created_on]
       ,t.[updated_by]
       ,t.[updated_on]
    FROM {schema}.[twn_date_tracking] t
    LEFT JOIN {schema}.[twn_date_config] c 
      ON t.[twn_date_type_id] = c.[twn_date_id]
    WHERE t.[twn_id] = {twn_id}
    ORDER BY t.[twn_date];
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}

#' Insert a New Tracking Date Action
#'
#' Adds a specific tracked date or action baseline against a notice.
#'
#' @param action_data A named list containing `twn_id`, `twn_date_type_id`,
#' `twn_date`, `twn_date_notes`, and `created_by`.
#'
#' @export
db_insert_notice_action <- function(action_data) {
  log_event("Starting db_insert_notice_action")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_notice_action")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_date_tracking] (
        [twn_id]
       ,[twn_date_type_id]
       ,[twn_date]
       ,[twn_date_notes]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
    ) VALUES (
        {action_data$twn_id}
       ,{action_data$twn_date_type_id}
       ,{action_data$twn_date}
       ,{action_data$twn_date_notes}
       ,{action_data$created_by}
       ,GETDATE()
       ,{action_data$created_by}
       ,GETDATE()
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Tracking Date Action
#'
#' Modifies a tracked milestone row based on its unique tracking primary key (`twn_date_id`).
#'
#' @param twn_date_id Integer. The primary key of the targeted tracking record row.
#' @param update_data A named list containing `twn_date_type_id`, `twn_date`,
#' `twn_date_notes`, and `updated_by`.
#'
#' @export
db_update_notice_action <- function(twn_date_id, update_data) {
  log_event(paste(
    "Starting db_update_notice_action for Tracking Row ID:",
    twn_date_id
  ))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_notice_action")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_date_tracking]
    SET [twn_date_type_id] = {update_data$twn_date_type_id}
       ,[twn_date]         = {update_data$twn_date}
       ,[twn_date_notes]   = {update_data$twn_date_notes}
       ,[updated_by]       = {update_data$updated_by}
       ,[updated_on]       = GETDATE()
    WHERE [twn_date_id] = {twn_date_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Delete a Tracking Date Action Record
#'
#' Deletes a specific action date record permanently from the database.
#'
#' @param twn_date_id Integer. The tracking row ID to remove.
#'
#' @export
db_delete_notice_action <- function(twn_date_id) {
  log_event(paste(
    "Starting db_delete_notice_action for Tracking Row ID:",
    twn_date_id
  ))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_delete_notice_action")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    DELETE FROM {schema}.[twn_date_tracking]
    WHERE [twn_date_id] = {twn_date_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}
