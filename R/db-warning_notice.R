#' Retrieve All Notices with Configurations Joined
#'
#' Returns all records from the `twn_all_notices` table, joining status and type
#' configurations to pull in human-readable names and descriptions.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing all notice records with joined status
#' and type dimensions.
#'
#' @export
db_get_all_notices <- function() {
  log_event("Starting db_get_all_notices")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_all_notices")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    SELECT 
        n.[twn_id]
       ,n.[urn]
       ,n.[school_name]
       ,n.[school_la]
       ,n.[school_region]
       ,n.[school_religious_character]
       ,n.[school_diocesce]
       ,n.[school_date_joined_trust]
       ,n.[school_status]
       ,n.[trust_ref]
       ,n.[trust_name]
       ,n.[trust_type]
       ,n.[trust_region]
       ,n.[trust_relationship_manager]
       ,n.[inspection_id]
       ,n.[inspection_date]
       ,n.[type_of_notice_id]
       ,t.[twn_type_name]
       ,t.[twn_type_desc]
       ,n.[twn_status_id]
       ,s.[twn_status_name]
       ,s.[twn_status_desc]
       ,n.[exemption_applied]
       ,n.[created_by]
       ,n.[created_on]
       ,n.[updated_by]
       ,n.[updated_on]
       ,n.[delivery_officer]
    FROM {schema}.[twn_all_notices] n
    LEFT JOIN {schema}.[twn_status_config] s 
      ON n.[twn_status_id] = s.[twn_status_id]
    LEFT JOIN {schema}.[twn_type_config] t 
      ON n.[type_of_notice_id] = t.[twn_type_id];
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}

#' Insert a New Notice
#'
#' Inserts a new record into the `twn_all_notices` table.
#'
#' @param notice_data A named list or row of a data frame containing fields matching
#' the table columns. Missing metadata fields like `created_on` default to `GETDATE()`.
#'
#' @return The result of the execution (typically number of rows affected).
#'
#' @export
db_insert_notice <- function(notice_data) {
  log_event("Starting db_insert_notice")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_notice")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_all_notices] (
        [urn]
       ,[school_name]
       ,[school_la]
       ,[school_region]
       ,[school_religious_character]
       ,[school_diocesce]
       ,[school_date_joined_trust]
       ,[school_status]
       ,[trust_ref]
       ,[trust_name]
       ,[trust_type]
       ,[trust_region]
       ,[trust_relationship_manager]
       ,[inspection_id]
       ,[inspection_date]
       ,[type_of_notice_id]
       ,[twn_status_id]
       ,[exemption_applied]
       ,[created_by]
       ,[created_on]
       ,[updated_by]
       ,[updated_on]
       ,[delivery_officer]
    ) VALUES (
        {notice_data$urn}
       ,{notice_data$school_name}
       ,{notice_data$school_la}
       ,{notice_data$school_region}
       ,{notice_data$school_religious_character}
       ,{notice_data$school_diocesce}
       ,{notice_data$school_date_joined_trust}
       ,{notice_data$school_status}
       ,{notice_data$trust_ref}
       ,{notice_data$trust_name}
       ,{notice_data$trust_type}
       ,{notice_data$trust_region}
       ,{notice_data$trust_relationship_manager}
       ,{notice_data$inspection_id}
       ,{notice_data$inspection_date}
       ,{notice_data$type_of_notice_id}
       ,{notice_data$twn_status_id}
       ,{notice_data$exemption_applied}
       ,{notice_data$created_by}
       ,GETDATE()
       ,{notice_data$updated_by}
       ,GETDATE()
       ,{notice_data$delivery_officer}
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Notice Record
#'
#' Updates all modifiable fields for a specific notice identified by `twn_id`.
#'
#' @param twn_id Integer. The unique identifier of the notice record to update.
#' @param update_data A named list containing the keys and new values to update.
#'
#' @return The result of the execution.
#'
#' @export
db_update_notice <- function(twn_id, update_data) {
  log_event(paste("Starting db_update_notice for ID:", twn_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_notice")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_all_notices]
    SET [urn]                        = {update_data$urn}
       ,[school_name]                = {update_data$school_name}
       ,[school_la]                  = {update_data$school_la}
       ,[school_region]              = {update_data$school_region}
       ,[school_religious_character] = {update_data$school_religious_character}
       ,[school_diocesce]            = {update_data$school_diocesce}
       ,[school_date_joined_trust]   = {update_data$school_date_joined_trust}
       ,[school_status]              = {update_data$school_status}
       ,[trust_ref]                  = {update_data$trust_ref}
       ,[trust_name]                 = {update_data$trust_name}
       ,[trust_type]                 = {update_data$trust_type}
       ,[trust_region]               = {update_data$trust_region}
       ,[trust_relationship_manager] = {update_data$trust_relationship_manager}
       ,[inspection_id]              = {update_data$inspection_id}
       ,[inspection_date]            = {update_data$inspection_date}
       ,[type_of_notice_id]          = {update_data$type_of_notice_id}
       ,[twn_status_id]              = {update_data$twn_status_id}
       ,[exemption_applied]          = {update_data$exemption_applied}
       ,[updated_by]                 = {update_data$updated_by}
       ,[updated_on]                 = GETDATE()
       ,[delivery_officer]           = {update_data$delivery_officer}
    WHERE [twn_id] = {twn_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}
