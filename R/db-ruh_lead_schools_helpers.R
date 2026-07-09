#' Retrieve Lead Support Master Ledger Catalog
#'
#' Retrieves the core lead support records with optional context filtering across Hubs or Events.
#'
#' @param ruhls_id Optional integer filter for a specific assignment ID.
#' @param hub_id Optional integer filter for a specific Hub context.
#' @param event_id Optional integer filter for a specific Event context.
#' @param entity_type Optional character string filter for the provider type (e.g., 'School', 'Trust').
#' @param entity_id Optional integer filter for the provider's unique ID/URN.
#' @param db_get_query Function used to execute the query, defaults to utils_db_get_query.
#' @export
db_ruh_get_lead_support_records <- function(
  ruhls_id = NULL,
  hub_id = NULL,
  event_id = NULL,
  entity_type = NULL,
  entity_id = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_lead_support_records")

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  conditions <- character()
  if (!is.null(ruhls_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruhls_id] = {as.integer(ruhls_id)}", .con = conn)
    )
  }
  if (!is.null(hub_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruhb_id] = {as.integer(hub_id)}", .con = conn)
    )
  }
  if (!is.null(event_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruev_id] = {as.integer(event_id)}", .con = conn)
    )
  }
  if (!is.null(entity_type)) {
    conditions <- c(
      conditions,
      glue_sql("l.[lead_entity_type] = {entity_type}", .con = conn)
    )
  }
  if (!is.null(entity_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[lead_entity_id] = {as.integer(entity_id)}", .con = conn)
    )
  }

  where_clause <- if (length(conditions) > 0) {
    glue_sql(
      "WHERE {DBI::SQL(paste(conditions, collapse = ' AND '))}",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT l.[ruhls_id], l.[lead_entity_type], l.[lead_entity_id], l.[ruhb_id], l.[ruev_id],
           l.[ruhl_dateactive], l.[ruhl_dateended], l.[ruhl_active], l.[ruhl_comment],
           l.[date_created], l.[user_id_created], l.[date_edited], l.[user_id_edited],
           h.[ruhb_name] AS [hub_name],
           e.[ruev_summary_notes] AS [event_summary]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_support_records] l
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h ON l.[ruhb_id] = h.[ruhb_id]
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_events] e ON l.[ruev_id] = e.[ruev_id]
    {where_clause}
    ORDER BY l.[ruhl_active] DESC, l.[ruhl_dateactive] DESC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a Flexible Lead Support Assignment Instance
#'
#' @param lead_type Character string defining the provider type ('School', 'Trust', etc.).
#' @param lead_id Integer tracking URN or ID of the provider.
#' @param hub_id Optional integer reference to a Hub context.
#' @param event_id Optional integer reference to an Event context.
#' @param user_id Character string identifier of the editing user.
#' @param date_start Character string or Date representing the start of the assignment.
#' @param comment Optional character string narrative comments.
#' @param db_get_query Function used to execute the query, defaults to utils_db_get_query.
#' @export
db_ruh_add_blank_lead_support <- function(
  lead_type,
  lead_id,
  hub_id = NULL,
  event_id = NULL,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue(
    "Adding flexible lead provider ledger row for identity type: {lead_type} ID: {lead_id}"
  ))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  h_val <- if (is.null(hub_id)) DBI::SQL("NULL") else as.integer(hub_id)
  e_val <- if (is.null(event_id)) DBI::SQL("NULL") else as.integer(event_id)
  comm_val <- if (is.null(comment) || !nzchar(trimws(comment))) {
    DBI::SQL("NULL")
  } else {
    comment
  }

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_lead_support_records] (
      [lead_entity_type], [lead_entity_id], [ruhb_id], [ruev_id],
      [ruhl_dateactive], [ruhl_active], [ruhl_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhls_id]
    VALUES ({lead_type}, {as.integer(lead_id)}, {h_val}, {e_val}, {date_start}, 1, {comm_val}, SYSUTCDATETIME(), {user_id});
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Master Lead Support Assignment Ledger
#' @export
db_ruh_update_lead_support <- function(
  ruhls_id,
  lead_type,
  lead_id,
  hub_id = NULL,
  event_id = NULL,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  h_val <- if (is.null(hub_id)) DBI::SQL("NULL") else as.integer(hub_id)
  e_val <- if (is.null(event_id)) DBI::SQL("NULL") else as.integer(event_id)
  ended_val <- if (is.null(date_ended) || date_ended == "") {
    DBI::SQL("NULL")
  } else {
    date_ended
  }
  comm_val <- if (is.null(comment) || !nzchar(trimws(comment))) {
    DBI::SQL("NULL")
  } else {
    comment
  }

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_lead_support_records]
    SET [lead_entity_type] = {lead_type},
        [lead_entity_id]   = {as.integer(lead_id)},
        [ruhb_id]          = {h_val},
        [ruev_id]          = {e_val},
        [ruhl_dateactive]  = {date_active},
        [ruhl_dateended]   = {ended_val},
        [ruhl_active]      = {as.integer(is_active)},
        [ruhl_comment]     = {comm_val},
        [date_edited]      = SYSUTCDATETIME(),
        [user_id_edited]   = {user_id}
    WHERE [ruhls_id]       = {as.integer(ruhls_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
  invisible(TRUE)
}

#' Get Cohorts Assigned to a Specific Lead Support Record
#' @export
db_ruh_get_lead_cohorts <- function(
  ruhls_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT [ruhlc_id], [cohort_id] FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_cohorts] WHERE [ruhls_id] = {as.integer(ruhls_id)};",
    .con = conn
  )
  db_get_query(conn, query)
}

#' Set Cohorts for a Lead Support Record (Sync Ledger)
#' @export
db_ruh_set_lead_cohorts <- function(
  ruhls_id,
  cohort_ids,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  # Clear existing assignments for sync transaction
  clear_query <- glue_sql(
    "DELETE FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_cohorts] WHERE [ruhls_id] = {as.integer(ruhls_id)};",
    .con = conn
  )
  db_get_query(conn, clear_query)

  if (length(cohort_ids) > 0) {
    values <- sapply(cohort_ids, function(cid) {
      glue_sql("({as.integer(ruhls_id)}, {as.integer(cid)})", .con = conn)
    })
    insert_query <- paste0(
      "INSERT INTO ",
      utils_resolve_schema('db_schema_01r'),
      ".[ruh_lead_cohorts] ([ruhls_id], [cohort_id]) VALUES ",
      paste(values, collapse = ", "),
      ";"
    )
    db_get_query(conn, DBI::SQL(insert_query))
  }
  invisible(TRUE)
}

#' Get Assigned Granular Support Records (Receivers)
#' @export
db_ruh_get_assigned_support_records <- function(
  ruhls_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    SELECT j.[ruhlasr_id], j.[ruhsr_id], r.[ruhsr_entity_type], r.[ruhsr_entity_id], t.[ruht_name] AS [support_type_name]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_assigned_support_records] j
    INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_records] r ON j.[ruhsr_id] = r.[ruhsr_id]
    INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t ON r.[ruht_id] = t.[ruht_id]
    WHERE j.[ruhls_id] = {as.integer(ruhls_id)};
    ",
    .con = conn
  )
  db_get_query(conn, query)
}

#' Assign Target Support Records to Provider Context
#' @export
db_ruh_set_assigned_support_records <- function(
  ruhls_id,
  ruhsr_ids,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  clear_query <- glue_sql(
    "DELETE FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_assigned_support_records] WHERE [ruhls_id] = {as.integer(ruhls_id)};",
    .con = conn
  )
  db_get_query(conn, clear_query)

  if (length(ruhsr_ids) > 0) {
    values <- sapply(ruhsr_ids, function(rid) {
      glue_sql("({as.integer(ruhls_id)}, {as.integer(rid)})", .con = conn)
    })
    insert_query <- paste0(
      "INSERT INTO ",
      utils_resolve_schema('db_schema_01r'),
      ".[ruh_lead_assigned_support_records] ([ruhls_id], [ruhsr_id]) VALUES ",
      paste(values, collapse = ", "),
      ";"
    )
    db_get_query(conn, DBI::SQL(insert_query))
  }
  invisible(TRUE)
}

#' Get Distinct Lead School Identifier Values Array (Legacy Compatibility Vector)
#' @export
db_get_hub_lead_urns <- function(db_get_query = utils_db_get_query) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT DISTINCT [lead_entity_id] AS [URN] FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_support_records] WHERE [lead_entity_type] = 'School';",
    .con = conn
  )
  db_get_query(conn, query)
}
