#' Retrieve Lead School Records Catalog
#'
#' @export
db_ruh_get_lead_schools <- function(
  ruhl_id = NULL,
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_lead_schools")

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  conditions <- character()
  if (!is.null(ruhl_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruhl_id] = {as.integer(ruhl_id)}", .con = conn)
    )
  }
  if (!is.null(hub_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruhb_id] = {as.integer(hub_id)}", .con = conn)
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
    SELECT l.[ruhl_id], l.[ruhb_id], l.[ruhl_urn], l.[ruhl_dateactive], l.[ruhl_dateended], 
           l.[ruhl_active], l.[ruhl_comment], l.[date_created], l.[user_id_created], l.[date_edited], l.[user_id_edited],
           h.[ruhb_name] AS [hub_name]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_schools] l
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h ON l.[ruhb_id] = h.[ruhb_id]
    {where_clause}
    ORDER BY l.[ruhl_active] DESC, l.[ruhl_dateactive] DESC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a Linked Lead School Hub Center
#' @export
db_ruh_add_blank_lead_school <- function(
  hub_id,
  urn,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue(
    "Adding explicit lead school center link context for URN: {urn}"
  ))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  comm_val <- if (is.null(comment) || !nzchar(trimws(comment))) {
    DBI::SQL("NULL")
  } else {
    comment
  }

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_lead_schools] (
      [ruhb_id], [ruhl_urn], [ruhl_dateactive], [ruhl_active], [ruhl_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhl_id]
    VALUES ({as.integer(hub_id)}, {as.integer(urn)}, {date_start}, 1, {comm_val}, SYSUTCDATETIME(), {user_id});
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Linked Lead School Assignment
#' @export
db_ruh_update_lead_school <- function(
  ruhl_id,
  hub_id,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

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
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_lead_schools]
    SET [ruhb_id]         = {as.integer(hub_id)},
        [ruhl_dateactive] = {date_active},
        [ruhl_dateended]  = {ended_val},
        [ruhl_active]     = {as.integer(is_active)},
        [ruhl_comment]    = {comm_val},
        [date_edited]     = SYSUTCDATETIME(),
        [user_id_edited]  = {user_id}
    WHERE [ruhl_id]       = {as.integer(ruhl_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
  invisible(TRUE)
}

#' Get Distinct Lead School Identifier Values Array
#' @export
db_get_hub_lead_urns <- function(db_get_query = utils_db_get_query) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT DISTINCT [ruhl_urn] AS [URN] FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_schools];",
    .con = conn
  )
  db_get_query(conn, query)
}
