#' Get Support Records with Polymorphic Lookups
#'
#' @param ruhsr_id Integer scalar or `NULL`. Unique primary track ID filter.
#' @param hub_id Integer scalar or `NULL`. Filter for a specific parent hub.
#' @param entity_id Character or Integer scalar or `NULL`. Target unit unique identifier.
#' @param entity_type Character scalar or `NULL`. Target category (e.g., 'School', 'Trust').
#' @param db_get_query Function used to execute the query (default: `utils_db_get_query`).
#' @export
db_ruh_get_support_records <- function(
  ruhsr_id = NULL,
  hub_id = NULL,
  entity_id = NULL,
  entity_type = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_support_records")
  result <- data.frame()

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(glue::glue(
        "Finished db_ruh_get_support_records (rows_returned = {nrow(result)})"
      ))
    },
    add = TRUE
  )

  conditions <- character()
  if (!is.null(ruhsr_id)) {
    conditions <- c(
      conditions,
      glue_sql("s.[ruhsr_id] = {ruhsr_id}", .con = conn)
    )
  }
  if (!is.null(hub_id)) {
    conditions <- c(conditions, glue_sql("s.[ruhb_id] = {hub_id}", .con = conn))
  }
  if (!is.null(entity_id)) {
    conditions <- c(
      conditions,
      # FIX: Apply character conversion cast to underlying entity support records ledger identification column rules
      glue_sql(
        "CAST(s.[ruhsr_entity_id] AS VARCHAR(50)) = {as.character(entity_id)}",
        .con = conn
      )
    )
  }
  if (!is.null(entity_type)) {
    conditions <- c(
      conditions,
      glue_sql(
        "s.[ruhsr_entity_type] = {as.character(entity_type)}",
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
    SELECT s.[ruhsr_id], s.[ruhl_id], s.[ruhb_id], s.[ruht_id], 
           s.[ruhsr_entity_type], s.[ruhsr_entity_id], 
           s.[ruhsr_dateactive], s.[ruhsr_dateended], s.[ruhsr_active], s.[ruhsr_comment],
           s.[date_created], s.[user_id_created], s.[date_edited], s.[user_id_edited],
           h.[ruhb_name], 
           t.[ruht_name] AS [support_type_name]
    FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_support_records] s
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h 
      ON s.[ruhb_id] = h.[ruhb_id]
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t 
      ON s.[ruht_id] = t.[ruht_id]
    {where_clause};
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  result
}

#' Add a Blank Polymorphic Support Record Link
#'
#' Initializes an active placeholder provisioning framework record for an entity tier.
#'
#' @param hub_id Integer scalar. Structural parent Hub container reference.
#' @param ruht_id Integer scalar. Categorized track option identifier.
#' @param entity_id Integer scalar. Unique URN, Trust ID, LA Code, or Diocese ID.
#' @param entity_type Character scalar. Context label ('School', 'Trust', 'LA', 'Diocese').
#' @param lead_school_id Integer scalar or `NULL`. Optional oversight school relationship link.
#' @param user_id Character scalar. Audit accountability tracking identifier.
#' @param date_start Date scalar or String 'YYYY-MM-DD'. Commencement calendar mark.
#' @param comment Character scalar or `NULL`. Descriptive tracking annotation block.
#' @param db_get_query Function used to execute the transaction statement.
#' @return Integer scalar. The newly generated `ruhsr_id` primary key record ID.
#' @export
db_ruh_add_blank_support_record <- function(
  hub_id,
  ruht_id,
  entity_id,
  entity_type,
  lead_school_id = NULL,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue(
    "Adding polymorphic track entry for {entity_type} ID: {entity_id}"
  ))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  lead_val <- if (is.null(lead_school_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(lead_school_id)
  }

  query <- glue_sql(
    "
    INSERT INTO  {utils_resolve_schema('db_schema_01r')}.[ruh_support_records] (
      [ruhl_id], [ruhb_id], [ruht_id], [ruhsr_entity_type], [ruhsr_entity_id], 
      [ruhsr_dateactive], [ruhsr_active], [ruhsr_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhsr_id]
    VALUES (
      {lead_val}, {as.integer(hub_id)}, {as.integer(ruht_id)}, {entity_type}, {as.integer(entity_id)}, 
      {date_start}, 1, {comment %||% DBI::SQL('NULL')}, SYSUTCDATETIME(), {user_id}
    );
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Polymorphic Support Record
#'
#' Writes sub-form transaction edits securely back to the operational database tracking framework.
#'
#' @param ruhsr_id Integer scalar. The master primary key of the support record row.
#' @param hub_id Integer scalar. Parent structural Hub reference.
#' @param ruht_id Integer scalar. Target support track category configuration.
#' @param lead_school_id Integer scalar or `NULL`. Optional supervisory oversight link.
#' @param date_active Date/Character scalar. Calendar mark tracking when provision began.
#' @param date_ended Date/Character scalar or `NULL`. Calendar mark tracking when assignment closed.
#' @param is_active Integer/Logical scalar. Active status check configuration flag.
#' @param comment Character scalar or `NULL`. Text field annotations summary.
#' @param user_id Character scalar. Accountability audit tracking string.
#' @param db_get_query Function used to execute the updates command.
#' @export
db_ruh_update_support_record <- function(
  ruhsr_id,
  hub_id,
  ruht_id,
  lead_school_id = NULL,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Updating support record database ID: {ruhsr_id}"))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  lead_val <- if (is.null(lead_school_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(lead_school_id)
  }
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
    UPDATE  {utils_resolve_schema('db_schema_01r')}.[ruh_support_records]
    SET 
      [ruhl_id]           = {lead_val},
      [ruhb_id]           = {as.integer(hub_id)},
      [ruht_id]           = {as.integer(ruht_id)},
      [ruhsr_dateactive]  = {date_active},
      [ruhsr_dateended]   = {ended_val},
      [ruhsr_active]      = {as.integer(is_active)},
      [ruhsr_comment]     = {comm_val},
      [date_edited]       = SYSUTCDATETIME(),
      [user_id_edited]    = {user_id}
    WHERE [ruhsr_id]      = {as.integer(ruhsr_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
  invisible(TRUE)
}

#' Get Unique Target IDs Currently Engaged inside a Provision Track
#'
#' @param entity_type Character scalar. Category type layer filter (e.g., 'School', 'Trust').
#' @export
db_get_engaged_entity_ids <- function(
  entity_type,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue(
    "Starting db_get_engaged_entity_ids for type: {entity_type}"
  ))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT DISTINCT [ruhsr_entity_id] AS [ID] 
     FROM {utils_resolve_schema('db_schema_01r')}.[ruh_support_records]
     WHERE [ruhsr_entity_type] = {entity_type};",
    .con = conn
  )
  db_get_query(conn, query)
}


# =================================================================================
# DEPRECATION STUBS & BACKWARDS-COMPATIBILITY LAYER
# =================================================================================

#' Get Support Schools (Deprecated)
#' @export
db_ruh_get_support_schools <- function(
  ruhs_id = NULL,
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  .Deprecated(
    "db_ruh_get_support_records",
    msg = "Use db_ruh_get_support_records instead."
  )
  res <- db_ruh_get_support_records(
    ruhsr_id = ruhs_id,
    hub_id = hub_id,
    entity_type = "School",
    db_get_query = db_get_query
  )
  if (nrow(res) > 0) {
    names(res)[names(res) == "ruhsr_id"] <- "ruhs_id"
    names(res)[names(res) == "ruhsr_entity_id"] <- "ruhs_urn"
    names(res)[names(res) == "ruhsr_dateactive"] <- "ruhs_dateactive"
    names(res)[names(res) == "ruhsr_dateended"] <- "ruhs_dateended"
    names(res)[names(res) == "ruhsr_active"] <- "ruhs_active"
    names(res)[names(res) == "ruhsr_comment"] <- "ruhs_comment"
    names(res)[names(res) == "support_type_name"] <- "ruht_name"
  }
  return(res)
}

#' Add Blank Support School (Deprecated)
#' @export
db_ruh_add_blank_support_school <- function(
  hub_id,
  ruht_id,
  urn,
  lead_school_id = NULL,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  .Deprecated(
    "db_ruh_add_blank_support_record",
    msg = "Use db_ruh_add_blank_support_record instead."
  )
  db_ruh_add_blank_support_record(
    hub_id = hub_id,
    ruht_id = ruht_id,
    entity_id = urn,
    entity_type = "School",
    lead_school_id = lead_school_id,
    user_id = user_id,
    date_start = date_start,
    comment = comment,
    db_get_query = db_get_query
  )
}

#' Update Support School (Deprecated)
#' @export
db_ruh_update_support_school <- function(
  ruhs_id,
  hub_id,
  ruht_id,
  lead_school_id = NULL,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  .Deprecated(
    "db_ruh_update_support_record",
    msg = "Use db_ruh_update_support_record instead."
  )
  db_ruh_update_support_record(
    ruhsr_id = ruhs_id,
    hub_id = hub_id,
    ruht_id = ruht_id,
    lead_school_id = lead_school_id,
    date_active = date_active,
    date_ended = date_ended,
    is_active = is_active,
    comment = comment,
    user_id = user_id,
    db_get_query = db_get_query
  )
}

#' Get Hub Support URNs (Deprecated)
#' @export
db_get_hub_support_urns <- function(db_get_query = utils_db_get_query) {
  .Deprecated(
    "db_get_engaged_entity_ids",
    msg = "Use db_get_engaged_entity_ids('School') instead."
  )
  res <- db_get_engaged_entity_ids(
    entity_type = "School",
    db_get_query = db_get_query
  )
  names(res)[names(res) == "ID"] <- "URN"
  return(res)
}
