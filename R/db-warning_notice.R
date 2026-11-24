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

#' Retrieve Scraped Post Notices
#'
#' Returns records from the `twn_post_notices` staging table. Can optionally be
#' filtered by a specific `twn_id` using a dynamic SQL filter.
#'
#' @param twn_id Optional Integer. If provided, filters the results to match the specified
#' notice identifier. Defaults to `NULL`.
#'
#' @export
db_get_scraped_posts <- function(twn_id = NULL) {
  log_event(paste(
    "Starting db_get_scraped_posts",
    if (!is.null(twn_id)) paste("for TWN ID:", twn_id) else ""
  ))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_scraped_posts")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  # Build the conditional WHERE clause snippet safely
  twn_id_check <- if (!is.null(twn_id)) {
    glue::glue_sql("WHERE [twn_id] = {twn_id}", .con = conn)
  } else {
    DBI::SQL("") # Returns a safe, empty SQL object
  }

  # Clean, unified query with the injected snippet
  query <- glue_sql(
    "
    SELECT 
        [post_twn_id]
       ,[urn]
       ,[twn_id]
       ,[school_name]
       ,[la]
       ,[region]
       ,[reason]
       ,[last_scraped_post]
       ,[first_scraped_post]
       ,[region_link]
       ,[school_link]
       ,[published_date]
    FROM {schema}.[twn_post_notices]
    {twn_id_check}
    ORDER BY [published_date] DESC;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}

#' Insert a Scraped Post Notice Row
#'
#' Writes a newly discovered scraped web portal notice entry into the staging table.
#'
#' @param post_data A named list containing fields matching the table schema columns.
#'
#' @return The result of the execution.
#'
#' @export
db_insert_scraped_post <- function(post_data) {
  log_event("Starting db_insert_scraped_post")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_insert_scraped_post")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    INSERT INTO {schema}.[twn_post_notices] (
        [urn]
       ,[twn_id]
       ,[school_name]
       ,[la]
       ,[region]
       ,[reason]
       ,[last_scraped_post]
       ,[first_scraped_post]
       ,[region_link]
       ,[school_link]
       ,[published_date]
    ) VALUES (
        {post_data$urn}
       ,{post_data$twn_id}
       ,{post_data$school_name}
       ,{post_data$la}
       ,{post_data$region}
       ,{post_data$reason}
       ,{post_data$last_scraped_post}
       ,{post_data$first_scraped_post}
       ,{post_data$region_link}
       ,{post_data$school_link}
       ,{post_data$published_date}
    );
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Update an Existing Scraped Post Notice
#'
#' Updates fields for a specific scraped post based on its primary key (`post_twn_id`).
#' Useful for updating the `last_scraped_post` date or links during re-scrapes.
#'
#' @param post_twn_id Integer. The unique key of the scraped post row to modify.
#' @param update_data A named list containing updated fields to patch.
#'
#' @return The result of the execution.
#'
#' @export
db_update_scraped_post <- function(post_twn_id, update_data) {
  log_event(paste("Starting db_update_scraped_post for Post ID:", post_twn_id))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_scraped_post")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_post_notices]
    SET [last_scraped_post]   = {update_data$last_scraped_post}
    WHERE [post_twn_id] = {post_twn_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Bind a Master ID to a Scraped Staging Record
#'
#' Updates only the `twn_id` relationship linkage field for a staging entry.
#'
#' @param post_twn_id Integer. The unique row ID of the staging table.
#' @param twn_id Integer. The primary key ID of the master notice entry.
#'
#' @export
db_link_post_to_master <- function(post_twn_id, twn_id) {
  log_event(paste(
    "Linking Staging Post ID:",
    post_twn_id,
    "to Master ID:",
    twn_id
  ))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_link_post_to_master")
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01a")

  query <- glue_sql(
    "
    UPDATE {schema}.[twn_post_notices]
    SET [twn_id] = {twn_id}
    WHERE [post_twn_id] = {post_twn_id};
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
}

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
