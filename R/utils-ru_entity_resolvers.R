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
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  meta_query <- glue_sql(
    "SELECT [ruet_source_table] FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_types] WHERE [ruet_type] = {entity_type};",
    .con = conn
  )
  meta <- db_get_query(conn, meta_query)
  if (nrow(meta) == 0) {
    return(data.frame(ID = integer(), Name = character(), Region = character()))
  }

  cols_query <- glue_sql(
    "SELECT [ruec_db_column], [ruec_friendly_name], [ruec_is_key] 
     FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_columns] 
     WHERE [ruec_entity_type] = {entity_type};",
    .con = conn
  )
  cols_df <- db_get_query(conn, cols_query)

  key_col <- cols_df$ruec_db_column[cols_df$ruec_is_key == 1][1]
  name_col <- cols_df$ruec_db_column[grepl(
    "name",
    tolower(cols_df$ruec_friendly_name)
  )][1]
  reg_col <- cols_df$ruec_db_column[grepl(
    "region",
    tolower(cols_df$ruec_friendly_name)
  )][1]

  select_name <- if (!is.na(name_col)) glue::glue("[{name_col}]") else "NULL"
  select_region <- if (!is.na(reg_col)) {
    glue::glue("[{reg_col}]")
  } else {
    "CAST('N/A' AS NVARCHAR)"
  }

  query <- glue_sql(
    "
    SELECT 
        {as.integer(entity_id)} AS [ID],
        {select_name}           AS [Name],
        {select_region}         AS [Region]
    FROM {utils_resolve_schema('db_schema_01r')}.{DBI::SQL(meta$ruet_source_table)}
    WHERE [{DBI::SQL(key_col)}] = {as.integer(entity_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
}
