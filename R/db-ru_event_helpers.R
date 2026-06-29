# =================================================================================
# 1. EVENT TYPES CONFIGURATION LOOKUPS
# =================================================================================

#' Retrieve Event Types Catalog
#'
#' @param ruevt_id Integer scalar or `NULL`. Filter for a specific event type ID.
#' @param db_get_query Function used to execute the query (default: `utils_db_get_query`).
#' @export
db_ru_get_event_types <- function(
  ruevt_id = NULL,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  filter_clause <- if (!is.null(ruevt_id)) {
    glue_sql("WHERE [ruevt_id] = {ruevt_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT [ruevt_id], [ruevt_name], [ruevt_description],
           [date_created], [user_id_created], [date_edited], [user_id_edited]
    FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_types]
    {filter_clause}
    ORDER BY [ruevt_name] ASC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a New Event Type Configuration Option
#'
#' @export
db_ru_add_event_type <- function(
  name,
  description = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  desc_val <- if (is.null(description) || !nzchar(trimws(description))) {
    DBI::SQL("NULL")
  } else {
    description
  }

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_types] (
      [ruevt_name], [ruevt_description], [date_created], [user_id_created]
    )
    OUTPUT INSERTED.[ruevt_id]
    VALUES ({name}, {desc_val}, SYSUTCDATETIME(), {user_id});
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Event Type Catalog Description
#'
#' @export
db_ru_update_event_type <- function(
  ruevt_id,
  name,
  description = NULL,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  desc_val <- if (is.null(description) || !nzchar(trimws(description))) {
    DBI::SQL("NULL")
  } else {
    description
  }

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_event_types]
    SET [ruevt_name]        = {name},
        [ruevt_description] = {desc_val},
        [date_edited]       = SYSUTCDATETIME(),
        [user_id_edited]    = {user_id}
    WHERE [ruevt_id]        = {as.integer(ruevt_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}


# =================================================================================
# 2. CORE EVENT RECORDS (POLYMORPHIC INTERACTION INSTANCES)
# =================================================================================

#' Get Event Registry Logs
#'
#' Pulls event interaction rows matching the active polymorphic entity target filters.
#'
#' @param ruev_id Integer scalar or `NULL`. Filter for an explicit event instance.
#' @param entity_id Integer scalar or `NULL`. Unique identifier target constraint.
#' @param entity_type Character scalar or `NULL`. Structural label filter (e.g., 'Trust').
#' @export
db_ru_get_events <- function(
  ruev_id = NULL,
  entity_id = NULL,
  entity_type = NULL,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  conditions <- character()
  if (!is.null(ruev_id)) {
    conditions <- c(
      conditions,
      glue_sql("e.[ruev_id] = {ruev_id}", .con = conn)
    )
  }
  if (!is.null(entity_id)) {
    conditions <- c(
      conditions,
      glue_sql("e.[ruev_entity_id] = {entity_id}", .con = conn)
    )
  }
  if (!is.null(entity_type)) {
    conditions <- c(
      conditions,
      glue_sql("e.[ruev_entity_type] = {entity_type}", .con = conn)
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
    SELECT e.[ruev_id], e.[ruevt_id], e.[ruev_entity_type], e.[ruev_entity_id],
           e.[ruev_date], e.[ruev_completed], e.[ruev_summary_notes],
           e.[date_created], e.[user_id_created], e.[date_edited], e.[user_id_edited],
           t.[ruevt_name] AS [event_type_name]
    FROM {utils_resolve_schema('db_schema_01r')}.[ru_events] e
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_types] t 
      ON e.[ruevt_id] = t.[ruevt_id]
    {where_clause}
    ORDER BY e.[ruev_date] DESC, e.[date_created] DESC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a Point-in-Time Interaction Log
#'
#' @export
db_ru_add_event <- function(
  event_type_id,
  event_sub_variety_id = 0,
  entity_id,
  entity_type,
  event_date,
  summary_notes = NULL,
  user_id = NULL
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_events] 
     ([ruevt_id], [ruesv_id], [ruev_entity_id], [ruev_entity_type], [ruev_date], [ruev_summary_notes], [user_id_created])
     OUTPUT INSERTED.[ruev_id]
     VALUES ({event_type_id}, {event_sub_variety_id}, {entity_id}, {entity_type}, {event_date}, {summary_notes}, {user_id});",
    .con = conn
  )

  DBI::dbGetQuery(conn, query)$ruev_id[1]
}

#' Update an Operational Event Record
#'
#' @export
db_ru_update_event <- function(
  ruev_id,
  event_date,
  is_completed,
  summary_notes = NULL,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  notes_val := if (is.null(summary_notes) || !nzchar(trimws(summary_notes))) {
    DBI::SQL("NULL")
  } else {
    summary_notes
  }

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_events]
    SET [ruev_date]          = {event_date},
        [ruev_completed]     = {as.integer(is_completed)},
        [ruev_summary_notes] = {notes_val},
        [date_edited]        = SYSUTCDATETIME(),
        [user_id_edited]     = {user_id}
    WHERE [ruev_id]          = {as.integer(ruev_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}


# =================================================================================
# 3. EVENT ACTIONS CONFIGURATION CATALOG (TABLE-DRIVEN METADATA MASTER CORES)
# =================================================================================

#' Get Pre-Configured Custom Event Fields Blueprint Definitions
#'
#' @export
db_ru_get_event_actions <- function(ruevt_id = 0, ruesv_id = 0) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
     WHERE ([ruevt_id] = {ruevt_id} AND [ruesv_id] = {ruesv_id})
        OR ([ruevt_id] = 0 AND [ruesv_id] = 0)
        OR ([ruevt_id] = {ruevt_id} AND [ruesv_id] = 0)
        OR ([ruevt_id] = 0 AND [ruesv_id] = {ruesv_id});",
    .con = conn
  )
  DBI::dbGetQuery(conn, query)
}

#' Add a Pre-Configured Custom Field Rule Blueprint Definition
#'
#' @export
db_ru_add_event_action <- function(
  event_type_id,
  action_name,
  description = NULL,
  rule_type,
  is_required = 0,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  desc_val <- if (is.null(description) || !nzchar(trimws(description))) {
    DBI::SQL("NULL")
  } else {
    description
  }

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] (
      [ruevt_id], [rueva_name], [rueva_description], [rueva_rule_type], [rueva_required], [date_created], [user_id_created]
    )
    OUTPUT INSERTED.[rueva_id]
    VALUES (
      {as.integer(event_type_id)}, {action_name}, {desc_val}, {rule_type}, {as.integer(is_required)}, SYSUTCDATETIME(), {user_id}
    );
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}


# =================================================================================
# 4. CUSTOM ACTION FORM INPUT RESPONSES LEDGER HANDLERS
# =================================================================================

#' Get Collected Dynamic Form Action Response Records
#'
#' Pulls configured fields paired alongside existing values logged for an event iteration.
#'
#' @export
db_ru_get_event_action_responses <- function(
  event_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    SELECT 
        a.[rueva_id],
        a.[rueva_name],
        a.[rueva_description],
        a.[rueva_rule_type],
        a.[rueva_required],
        r.[ruevar_id],
        r.[ruev_id],
        [ruevar_value] = ISNULL(r.[ruevar_value], '')
    FROM {utils_resolve_schema('db_schema_01r')}.[ru_events] e
    INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] a ON e.[ruevt_id] = a.[ruevt_id]
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses] r 
        ON e.[ruev_id] = r.[ruev_id] AND a.[rueva_id] = r.[rueva_id]
    WHERE e.[ruev_id] = {as.integer(event_id)}
    ORDER BY a.[rueva_id] ASC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Upsert or Commit a Ledger Response String Entry Value
#'
#' @export
db_ru_save_event_action_response <- function(
  event_id,
  rueva_id,
  response_value,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  val_string <- if (
    is.null(response_value) || !nzchar(trimws(as.character(response_value)))
  ) {
    DBI::SQL("NULL")
  } else {
    as.character(response_value)
  }

  check_query <- glue_sql(
    "SELECT [ruevar_id] FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses] 
     WHERE [ruev_id] = {as.integer(event_id)} AND [rueva_id] = {as.integer(rueva_id)};",
    .con = conn
  )
  existing <- db_get_query(conn, check_query)

  if (nrow(existing) > 0) {
    query <- glue_sql(
      "
      UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses]
      SET [ruevar_value]   = {val_string},
          [date_edited]    = SYSUTCDATETIME(),
          [user_id_edited] = {user_id}
      WHERE [ruevar_id]    = {existing$ruevar_id[1]};
      ",
      .con = conn
    )
  } else {
    query <- glue_sql(
      "
      INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses] (
        [ruev_id], [rueva_id], [ruevar_value], [date_created], [user_id_created]
      )
      VALUES (
        {as.integer(event_id)}, {as.integer(rueva_id)}, {val_string}, SYSUTCDATETIME(), {user_id}
      );
      ",
      .con = conn
    )
  }

  db_get_query(conn, query)
  invisible(TRUE)
}
