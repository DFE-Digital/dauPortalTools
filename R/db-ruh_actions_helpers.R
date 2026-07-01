#' Retrieve Action Configuration Catalog Definitions (Legacy Layer)
#' @export
db_ruh_get_actions <- function(
  ruha_id = NULL,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  action_filter <- if (!is.null(ruha_id)) {
    glue_sql("WHERE a.[ruha_id] = {as.integer(ruha_id)}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT a.[ruha_id], a.[ruhb_id], a.[ruht_id],
           ISNULL(h.[ruhb_name], 'Global Framework Scope') AS [hub_name],
           ISNULL(t.[ruht_name], 'Unassigned / General') AS [support_type_name],
           a.[ruha_name], a.[ruha_description],
           a.[date_created], a.[user_id_created], a.[date_edited], a.[user_id_edited]
    FROM {utils_resolve_schema('db_schema_01r')}.[ruh_actions] a
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h ON a.[ruhb_id] = h.[ruhb_id]
    LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t ON a.[ruht_id] = t.[ruht_id]
    {action_filter};
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a Pre-Configured Action Definition (Legacy Layer)
#' @export
db_ruh_add_action <- function(
  hub_id,
  ruht_id,
  action_name,
  description,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_actions] (
      [ruhb_id], [ruht_id], [ruha_name], [ruha_description], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruha_id]
    VALUES ({as.integer(hub_id)}, {as.integer(ruht_id)}, {action_name}, {description}, SYSUTCDATETIME(), {user_id});
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Action Catalog Item (Legacy Layer)
#' @export
db_ruh_update_action <- function(
  ruha_id,
  hub_id,
  ruht_id,
  action_name,
  description,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_actions]
    SET [ruhb_id]          = {as.integer(hub_id)},
        [ruht_id]          = {as.integer(ruht_id)},
        [ruha_name]        = {action_name}, 
        [ruha_description] = {description}, 
        [date_edited]      = SYSUTCDATETIME(), 
        [user_id_edited]   = {user_id}
    WHERE [ruha_id]        = {as.integer(ruha_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Register a New Hub Provision Sub-Category
#' @export
db_ruh_add_sub_category <- function(
  ruht_id,
  ruhb_id,
  name,
  description,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_sub_categories] 
      ([ruht_id], [ruhb_id], [ruhsc_name], [ruhsc_description], [created_by])
    VALUES 
      ({ruht_id}, {ruhb_id}, {name}, {trimws(description)}, {user_id});
  ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}

#' Register a New Hub Blueprint Input Field Configuration
#' @export
db_ruh_add_blueprint_field <- function(
  ruht_id,
  ruhsc_id,
  field_name,
  description,
  rule_type,
  is_required,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  # Structural server-side check ensuring incoming string types map strictly to your engine specs
  valid_types <- c("Character", "Integer", "Date", "Boolean")
  if (!rule_type %in% valid_types) {
    stop("Invalid storage validation type assignment.")
  }

  query <- glue::glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
      ([ruht_id], [ruhsc_id], [ruhbf_name], [ruhbf_description], [ruhbf_rule_type], [ruhbf_required], [created_by])
    VALUES
      ({ruht_id}, {ruhsc_id}, {field_name}, {trimws(description)}, {rule_type}, {is_required}, {user_id});
  ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
