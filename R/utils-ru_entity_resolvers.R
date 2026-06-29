#' Resolve Entity Summary Metadata Natively
#'
#' Fetches a standardized descriptive summary vector for a specific entity ID and type context.
#' Useful for analytical cards, headers, and dashboard labels.
#'
#' @param entity_id Integer scalar. URN, Trust ID, LA Code, or Diocese ID.
#' @param entity_type Character scalar ('School', 'Trust', 'LA', 'Diocese').
#' @param db_get_query Function used to execute the query.
#' @return A data.frame with uniform columns: ID, Name, Region.
#' @export
utils_ru_resolve_entity_meta <- function(
  entity_id,
  entity_type,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Resolving metadata for {entity_type} ID: {entity_id}"))

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  meta_query <- glue_sql(
    "SELECT [ruet_source_table] FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_types] WHERE [ruet_type] = {entity_type};",
    .con = conn
  )
  meta <- db_get_query(conn, meta_query)
  if (nrow(meta) == 0) {
    return(data.frame(ID = entity_id, Name = "Missing Type", Region = "N/A"))
  }

  cols_query <- glue_sql(
    "SELECT [ruec_db_column], [ruec_friendly_name], [ruec_is_key] 
     FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_columns] 
     WHERE [ruec_entity_type] = {entity_type}
     ORDER BY [ruec_column_id] ASC;",
    .con = conn
  )
  cols_df <- db_get_query(conn, cols_query)
  if (nrow(cols_df) == 0) {
    return(data.frame(ID = entity_id, Name = "Unconfigured", Region = "N/A"))
  }

  key_col <- cols_df$ruec_db_column[cols_df$ruec_is_key == 1][1]
  name_col <- cols_df$ruec_db_column[cols_df$ruec_is_key == 0][1]
  reg_col <- cols_df$ruec_db_column[cols_df$ruec_is_key == 0][2]

  select_name <- if (!is.na(name_col)) {
    DBI::SQL(glue::glue("[{name_col}]"))
  } else {
    DBI::SQL("CAST('Unknown' AS NVARCHAR)")
  }
  select_region <- if (!is.na(reg_col)) {
    DBI::SQL(glue::glue("[{reg_col}]"))
  } else {
    DBI::SQL("CAST('N/A' AS NVARCHAR)")
  }

  query <- glue_sql(
    "
    SELECT TOP 1
        [{DBI::SQL(key_col)}] AS [ID],
        {select_name}         AS [Name],
        {select_region}       AS [Region]
    FROM {utils_resolve_schema('db_schema_01r')}.[{DBI::SQL(meta$ruet_source_table)}]
    WHERE [{DBI::SQL(key_col)}] = {as.integer(entity_id)};
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)

  if (nrow(res) == 0) {
    return(data.frame(
      ID = entity_id,
      Name = "Not Found in Snapshot",
      Region = "N/A"
    ))
  }

  return(res)
}
