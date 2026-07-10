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
  description,
  user_id = NULL
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_event_types]
     SET [ruevt_name] = {name},
         [ruevt_description] = {description},
         [date_edited] = GETDATE(),
         [user_id_edited] = {user_id}
     WHERE [ruevt_id] = {as.integer(ruevt_id)};",
    .con = conn
  )
  utils_db_execute(conn, query)
}

# =================================================================================
# 2. CORE EVENT RECORDS (POLYMORPHIC INTERACTION INSTANCES)
# =================================================================================

#' Get Event Registry Logs
#'
#' Pulls event interaction rows matching the active polymorphic entity target filters.
#'
#' @param ruev_id Integer scalar or `NULL`. Filter for an explicit event instance.
#' @param entity_id Character or Integer scalar or `NULL`. Unique identifier target constraint.
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
      # FIX: Apply character conversion cast to underlying entity table identification column rules
      glue_sql(
        "CAST(e.[ruev_entity_id] AS VARCHAR(50)) = {as.character(entity_id)}",
        .con = conn
      )
    )
  }
  if (!is.null(entity_type)) {
    conditions <- c(
      conditions,
      glue_sql(
        "e.[ruev_entity_type] = {as.character(entity_type)}",
        .con = conn
      )
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
    SELECT e.[ruev_id], e.[ruevt_id], e.[ruesv_id], e.[ruev_entity_type], e.[ruev_entity_id],
           e.[ruev_date], e.[ruev_completed], e.[ruev_summary_notes],
           e.[date_created], e.[user_id_created], e.[date_edited], e.[user_id_edited],
           t.[ruevt_name]  AS [event_type_name],
           sv.[ruesv_name] AS [event_sub_variety_name]
    FROM {utils_resolve_schema('db_schema_01r')}.[ru_events] e
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_types] t 
      ON e.[ruevt_id] = t.[ruevt_id]
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_sub_varieties] sv
      ON e.[ruesv_id] = sv.[ruesv_id]
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
     VALUES ({event_type_id}, {event_sub_variety_id}, {as.character(entity_id)}, {entity_type}, {event_date}, {summary_notes}, {user_id});",
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

  notes_val <- if (is.null(summary_notes) || !nzchar(trimws(summary_notes))) {
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
# 3. EVENT ACTIONS CONFIGURATION CATALOG
# =================================================================================

#' Get Pre-Configured Custom Event Fields Blueprint Definitions
#'
#' @export
db_ru_get_event_actions <- function(ruevt_id = 0, ruesv_id = 0) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
     WHERE ([ruevt_id] = {as.integer(ruevt_id)} AND [ruesv_id] = {as.integer(ruesv_id)})
        OR ([ruevt_id] = 0 AND [ruesv_id] = 0) 
        OR ([ruevt_id] = {as.integer(ruevt_id)} AND [ruesv_id] = 0);",
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
  description = "",
  rule_type = "Character",
  is_required = 0,
  ruesv_id = 0,
  user_id = NULL
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
     ([ruevt_id], [ruesv_id], [rueva_name], [rueva_description], [rueva_rule_type], [rueva_required], [user_id_created])
     VALUES ({as.integer(event_type_id)}, {as.integer(ruesv_id)}, {action_name}, {description}, {rule_type}, {as.integer(is_required)}, {user_id});",
    .con = conn
  )

  utils_db_execute(conn, query)
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

#' Fetch Registered Sub-Varieties for an Event Type
#'
#' @param ruevt_id Integer scalar. Filter by parent type (optional).
#' @export
db_ru_get_event_sub_varieties <- function(ruevt_id = NULL) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  base_query <- "SELECT [ruesv_id], [ruevt_id], [ruesv_name], [ruesv_description], [date_created], [user_id_created] 
                 FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_sub_varieties]"

  if (!is.null(ruevt_id)) {
    query <- glue_sql(
      paste(
        base_query,
        "WHERE [ruevt_id] = {as.integer(ruevt_id)} ORDER BY [ruesv_name] ASC;"
      ),
      .con = conn
    )
  } else {
    query <- glue_sql(
      paste(base_query, "ORDER BY [ruesv_name] ASC;"),
      .con = conn
    )
  }

  DBI::dbGetQuery(conn, query)
}

#' Add a New Event Sub-Variety Entry
#'
#' @export
db_ru_add_event_sub_variety <- function(
  ruevt_id,
  name,
  description = NULL,
  user_id = NULL
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_sub_varieties]
     ([ruevt_id], [ruesv_name], [ruesv_description], [user_id_created])
     VALUES ({as.integer(ruevt_id)}, {name}, {description}, {user_id});",
    .con = conn
  )

  utils_db_execute(conn, query)
}

#' Retrieve Master Profile Metadata for a Point-in-Time Event
#'
#' Pulls record parameters for a specific primary event container tracker row.
#'
#' @param event_master_id Integer. The primary key ID of the target event master record.
#' @return A data.frame containing columns `ruevm_id`, `ruevm_name`, `ruevm_active`, `created_date`, and `created_by`.
#' @export
db_ru_get_event_master_record <- function(event_master_id) {
  req(event_master_id)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [ruevm_id], [ruevm_name], [ruevm_active], [created_date], [created_by]
     FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_master]
     WHERE [ruevm_id] = {as.integer(event_master_id)};",
    .con = conn
  )

  df <- DBI::dbGetQuery(conn, query)
  req(nrow(df) > 0)

  return(df)
}
