# ==============================================================================
# File: R/db-strategic_authority.R
# Purpose: Database query and mutation functions for Strategic Authority
# identification.
#
# Functions:
#   - db_get_strategic_authority_by_code() : Fetch all master user directory records
#   - db_get_strategic_authority_by_name() : Fetch user record by primary key user_id
#
# Dependencies: DBI, glue, internal
# ==============================================================================
#
#' Retrieve Strategic Authority Details by Local Authority Code
#'
#' Queries the strategic authority reference mapping table for one or more
#' Local Authority codes (3-digit DfE LEA codes).
#'
#' @param la_codes An integer or numeric vector of DfE Local Authority codes.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `la_code`, `la_name`,
#'   `combined_authority`, and `mayoral_strategic_authority`.
#' @export
db_get_strategic_authority_by_code <- function(la_codes) {
  log_event("Starting db_get_strategic_authority_by_code", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_strategic_authority_by_code")
    },
    add = TRUE
  )

  la_codes <- as.integer(unique(la_codes))

  query <- glue::glue_sql(
    "
    SELECT
      la_code,
      la_name,
      combined_authority,
      mayoral_strategic_authority
    FROM {utils_resolve_schema('db_schema_00cr')}.[la_to_strategic_authority]
    WHERE la_code IN ({la_codes*})
    ORDER BY la_name;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}


#' Retrieve Strategic Authority Details by Local Authority Name
#'
#' Queries the strategic authority reference mapping table for one or more
#' Local Authority names.
#'
#' @param la_names A character vector of Local Authority names.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `la_code`, `la_name`,
#'   `combined_authority`, and `mayoral_strategic_authority`.
#' @export
db_get_strategic_authority_by_name <- function(la_names) {
  log_event("Starting db_get_strategic_authority_by_name", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_strategic_authority_by_name")
    },
    add = TRUE
  )

  la_names <- as.character(unique(trimws(la_names)))

  query <- glue::glue_sql(
    "
    SELECT
      la_code,
      la_name,
      combined_authority,
      mayoral_strategic_authority
    FROM {utils_resolve_schema('db_schema_00cr')}.[la_to_strategic_authority]
    WHERE la_name IN ({la_names*})
    ORDER BY la_name;
    ",
    .con = conn
  )

  utils_db_get_query(conn, query)
}
