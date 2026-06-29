#' Retrieve Active Support Framework Provisioning Types
#' @export
db_ruh_get_support_types <- function(
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  where_clause <- if (!is.null(hub_id)) {
    glue_sql("WHERE [ruhb_id] IN (0, {as.integer(hub_id)})", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT [ruht_id], [ruhb_id], [ruht_name], [ruht_description],
           [date_created], [user_id_created], [date_edited], [user_id_edited]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_support_types]
    {where_clause}
    ORDER BY [ruhb_id] ASC, [ruht_name] ASC;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a New Support Framework Tracking Track
#' @export
db_ruh_add_support_type <- function(
  hub_id,
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
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] (
      [ruhb_id], [ruht_name], [ruht_description], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruht_id]
    VALUES ({as.integer(hub_id)}, {name}, {desc_val}, SYSUTCDATETIME(), {user_id});
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Support Track Configuration Blueprint
#' @export
db_ruh_update_support_type <- function(
  ruht_id,
  name,
  description = NULL,
  hub_id,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  desc_val <- if (is.null(description) || !nzchar(trimws(description))) {
    DBI::SQL("NULL")
  } else {
    description
  }

  query := glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_support_types]
    SET [ruht_name]        = {name}, 
        [ruht_description] = {desc_val}, 
        [ruhb_id]          = {as.integer(hub_id)},
        [date_edited]      = SYSUTCDATETIME(), 
        [user_id_edited]   = {user_id}
    WHERE [ruht_id]        = {as.integer(ruht_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
