#' Retrieve Hub Master Records
#'
#' Returns core tracking hubs from the master lookups schema catalog.
#'
#' @param hub_id Integer scalar or `NULL`. Optional explicit primary key filter.
#' @param db_get_query Function used to execute the query (default: `utils_db_get_query`).
#' @return A `data.frame` containing master hub structures.
#' @export
db_ruh_get_hubs <- function(hub_id = NULL, db_get_query = utils_db_get_query) {
  log_event("Starting db_ruh_get_hubs")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_ruh_get_hubs")
    },
    add = TRUE
  )

  hub_filter <- if (!is.null(hub_id)) {
    glue_sql("WHERE [ruhb_id] = {as.integer(hub_id)}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT [ruhb_id], [ruhb_name], 
           [date_created], [user_id_created], [date_edited], [user_id_edited]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_hubs]
    {hub_filter}
    ORDER BY [ruhb_name] ASC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a Primary Hub Container Profile Record
#'
#' @export
db_ruh_add_hub <- function(
  hub_name,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_add_hub")

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] (
      [ruhb_name], [user_id_created], [date_created]
    ) 
    OUTPUT INSERTED.[ruhb_id]
    VALUES ({hub_name}, {user_id}, SYSUTCDATETIME());
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Hub Container Name Attribute
#'
#' @export
db_ruh_update_hub <- function(hub_id, hub_name, user_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_hubs]
    SET [ruhb_name]      = {hub_name},
        [date_edited]    = SYSUTCDATETIME(),
        [user_id_edited] = {user_id}
    WHERE [ruhb_id]      = {as.integer(hub_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}

#' Retrieve Hub Performance Summary (Polymorphic Upgraded)
#'
#' Returns aggregated counts mapping active engagement allocations across the polymorphic track matrix.
#'
#' @export
db_ruh_get_hub_summary <- function() {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT 
         h.[ruhb_id] AS [id],
         h.[ruhb_name] AS [hub_name],
         COUNT(DISTINCT CASE WHEN sr.[ruhsr_active] = 1 AND sr.[ruhsr_entity_type] = 'School' THEN sr.[ruhsr_entity_id] END) AS [active_schools_tracked],
         COUNT(DISTINCT CASE WHEN sr.[ruhsr_active] = 1 AND sr.[ruhsr_entity_type] = 'Trust' THEN sr.[ruhsr_entity_id] END) AS [active_trusts_tracked],
         COUNT(DISTINCT CASE WHEN sr.[ruhsr_active] = 1 AND sr.[ruhsr_entity_type] = 'LA' THEN sr.[ruhsr_entity_id] END) AS [active_las_tracked],
         COUNT(DISTINCT CASE WHEN ls.[ruhl_active] = 1 THEN ls.[ruhl_id] END) AS [active_lead_centers],
         COUNT(DISTINCT st.[ruht_id]) AS [total_active_categories]
       FROM {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_records] sr 
         ON h.[ruhb_id] = sr.[ruhb_id]
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] ls 
         ON h.[ruhb_id] = ls.[ruhb_id]
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] st 
         ON h.[ruhb_id] = st.[ruhb_id]
       GROUP BY h.[ruhb_id], h.[ruhb_name]
       ORDER BY h.[ruhb_name] ASC;"
      )
      res <- DBI::dbGetQuery(conn, query)
      tibble::as_tibble(res)
    },
    error = function(e) {
      tibble::tibble(
        id = integer(0),
        hub_name = character(0),
        active_schools_tracked = integer(0),
        active_trusts_tracked = integer(0),
        active_las_tracked = integer(0),
        active_lead_centers = integer(0),
        total_active_categories = integer(0)
      )
    }
  )
}


#' Retrieve Master List of All Regional Hub Profiles
#'
#' Pulls a clean, unique data frame of all active hub IDs and names.
#'
#' @return A data.frame with columns `ruhb_id` and `ruhb_name`.
#' @export
db_hubs_lookup <- function() {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [ruhb_id], [ruhb_name] 
     FROM {utils_resolve_schema('db_schema_01r')}.[ruh_hubs]
     ORDER BY [ruhb_name];",
    .con = conn
  )

  DBI::dbGetQuery(conn, query)
}
