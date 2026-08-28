#' Get Dynamic Entity Data for Search Tables
#'
#' Queries the relational schema configuration tables to dynamically assemble
#' a standardized data frame for the search view matching the selected tier.
#'
#' @param selected_type Character scalar (e.g., 'School', 'Trust', 'LA', 'Diocese').
#' @param db_get_query Function used to execute the query (default: `utils_db_get_query`).
#' @return A `data.frame` containing columns matching the friendly headers configured in the DB.
#' @export

db_get_search_entities <- function(
  selected_type,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  meta <- db_get_query(
    conn,
    glue_sql(
      "SELECT [ruet_source_table] FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_types] WHERE [ruet_type] = {selected_type};",
      .con = conn
    )
  )
  if (nrow(meta) == 0) {
    return(data.frame())
  }

  cols_df <- db_get_query(
    conn,
    glue_sql(
      "SELECT [ruec_db_column], [ruec_friendly_name] FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_columns] WHERE [ruec_entity_type] = {selected_type} ORDER BY [ruec_column_id] ASC;",
      .con = conn
    )
  )
  if (nrow(cols_df) == 0) {
    return(data.frame())
  }

  select_clause <- paste(
    glue::glue("[{cols_df$ruec_db_column}] AS [{cols_df$ruec_friendly_name}]"),
    collapse = ", "
  )

  # Plain, fast select directly from the local view
  query <- glue_sql(
    "
    SELECT {DBI::SQL(select_clause)}
    FROM {utils_resolve_schema('db_schema_01r')}.{DBI::SQL(meta$ruet_source_table)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
}


#' Resolve the Unique Primary Identifier Label for a Selection
#'
#' Looks up the friendly header column name flagged as the master key row
#' for a given entity category (e.g., returns 'URN' for 'School').
#'
#' @param selected_type Character scalar (e.g., 'School', 'Trust').
#' @param db_get_query Function used to execute the query.
#' @return Character scalar representing the column header name.
#' @export
db_resolve_entity_key_label <- function(
  selected_type,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "SELECT [ruec_friendly_name] 
     FROM {utils_resolve_schema('db_schema_01r')}.[ru_entity_columns] 
     WHERE [ruec_entity_type] = {selected_type} AND [ruec_is_key] = 1;",
    .con = conn
  )
  res <- db_get_query(conn, query)

  if (nrow(res) == 0) {
    return(NULL)
  }
  res$ruec_friendly_name[1]
}

# =================================================================================
# DEPRECATION STUBS & BACKWARDS-COMPATIBILITY LAYER
# =================================================================================

#' Retrieve School List (Deprecated)
#'
#' @description This function is deprecated. Please update your references to use
#' \code{db_get_search_entities("School")} instead.
#' @export
db_get_school_list <- function(db_get_query = utils_db_get_query) {
  .Deprecated(
    "db_get_search_entities",
    msg = "Use db_get_search_entities('School') instead."
  )
  db_get_search_entities(selected_type = "School", db_get_query = db_get_query)
}
