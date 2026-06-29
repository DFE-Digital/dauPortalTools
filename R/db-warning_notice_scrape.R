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
