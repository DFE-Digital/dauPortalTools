# ==============================================================================
# File: R/db-dao.R
# Purpose: Database query functions for directive Academy Order (dAO) data
#
# Functions:
#   - db_dao_get_all()                 : Fetch full typed dAO history from SQL
#   - db_dao_get_latest_extract()      : Fetch typed latest monthly snapshot from SQL
#   - db_dao_get_historical_timeline() : Fetch 12-month caseload status series from SQL
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all directive Academy Order (dAO) records
#'
#' Retrieves the complete historical dataset from the `dAOList` table across all
#' extraction dates, standardising column names into snake_case and casting raw
#' database strings and timestamps into appropriate analytical types (`integer`,
#' `Date`, `logical`, and `numeric`).
#'
#' @details
#' Queries all rows from `[dAOList]` within the schema resolved by
#' `utils_resolve_schema("db_schema_00c")`.
#'
#' Type conversions are performed in SQL Server using `TRY_CAST()` for numeric
#' identifiers/counts and `CAST(... AS DATE)` for midnight-truncated
#' `datetime` fields. Categorical flag strings (`"Yes"`/`"No"` and
#' `"Over 9 months"`/`"Under 9 months"`) are coerced to SQL `BIT` so they
#' arrive in R as `logical` vectors.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (or `tibble`, depending on [utils_db_get_query()])
#'   containing all historical extract records with 17 typed columns, or `NULL`
#'   if a database connection or query error occurs:
#'   \describe{
#'     \item{`urn`}{Integer. School URN.}
#'     \item{`school_name`}{Character. Name of the establishment.}
#'     \item{`school_type`}{Character. Establishment type description.}
#'     \item{`local_authority`}{Character. Local authority name.}
#'     \item{`region`}{Character. DfE geographical region.}
#'     \item{`phase`}{Character. Phase of education (e.g., `"Primary"`, `"Secondary"`).}
#'     \item{`date_dao_approved`}{Date. Date directive Academy Order approval was granted.}
#'     \item{`date_dao_revoked`}{Date. Date dAO was revoked (`NULL` if active/converted).}
#'     \item{`sponsor_match`}{Logical. `TRUE` if matched to a sponsor (`"Yes"`), `FALSE` if `"No"`.}
#'     \item{`dao_status`}{Character. dAO status classification. One of:
#'       `"Closed"`, `"Awaiting conversion"`,
#'       `"Special Case - Will never be issued a dAO so keep approval date blank"`,
#'       `"dAO not issued"`, `"Revoked"`, or `"Closed to become an Academy"`.}
#'     \item{`lead`}{Character. Project Lead (`PL`) assigned to the case.}
#'     \item{`new_academy_urn`}{Integer. URN of the opened academy (`NULL` if not yet opened).}
#'     \item{`new_academy_open_date`}{Date. Academy opening date sourced from GIAS.}
#'     \item{`length_of_dao_months`}{Numeric. Duration to open or total length of dAO in months, rounded to 2 decimal places.}
#'     \item{`over_9_months_flag`}{Logical. `TRUE` if `"Over 9 months"`, `FALSE` if `"Under 9 months"`.}
#'     \item{`pupil_numbers`}{Integer. Pupil headcount on roll.}
#'     \item{`date_of_extract`}{Date. Snapshot extraction date.}
#'   }
#'
#' @family dAO database functions
#' @seealso [db_dao_get_latest_extract()], [db_dao_get_historical_timeline()],
#'   [sql_manager()], [utils_resolve_schema()], [log_event()]
#'
#' @export
#' @examples
#' \dontrun{
#' all_dao <- db_dao_get_all()
#'
#' # Using an alternative connection alias
#' all_dao_uat <- db_dao_get_all(conn_name = "dit_uat")
#' }
db_dao_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_00c")

  query <- glue::glue(
    "
    SELECT 
      TRY_CAST([URN] AS INT) AS [urn],
      [School Name] AS [school_name],
      [School Type] AS [school_type],
      [Local Authority] AS [local_authority],
      [Region] AS [region],
      [Phase] AS [phase],
      CAST([Date dAO approval granted] AS DATE) AS [date_dao_approved],
      CAST([Date dAO revoked] AS DATE) AS [date_dao_revoked],
      CASE [Sponsor Match?]
        WHEN 'Yes' THEN CAST(1 AS BIT)
        WHEN 'No'  THEN CAST(0 AS BIT)
        ELSE NULL
      END AS [sponsor_match],
      [dAO Status*] AS [dao_status],
      [PL] AS [lead],
      TRY_CAST([New Academy URN] AS INT) AS [new_academy_urn],
      CAST([New Academy Open date (from GIaS)] AS DATE) AS [new_academy_open_date],
      ROUND([How long to Open as Academy/Length of dAO (months)], 2) AS [length_of_dao_months],
      CASE [Over 9 months]
        WHEN 'Over 9 months'  THEN CAST(1 AS BIT)
        WHEN 'Under 9 months' THEN CAST(0 AS BIT)
        ELSE NULL
      END AS [over_9_months_flag],
      TRY_CAST([Pupil Numbers] AS INT) AS [pupil_numbers],
      [date_of_extract]
    FROM {schema}.[dAOList];
  "
  )

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch the latest directive Academy Order (dAO) snapshot
#'
#' Retrieves the most recent monthly extract from the `dAOList` table,
#' standardising column names into snake_case and casting raw database strings
#' and timestamps into appropriate analytical types (`integer`, `Date`,
#' `logical`, and `numeric`).
#'
#' @details
#' Queries `[dAOList]` within the schema resolved by
#' `utils_resolve_schema("db_schema_00c")`, filtering strictly to rows matching
#' `MAX([date_of_extract])`.
#'
#' Type conversions are performed in SQL Server using `TRY_CAST()` for numeric
#' identifiers/counts and `CAST(... AS DATE)` for midnight-truncated
#' `datetime` fields. Categorical flag strings (`"Yes"`/`"No"` and
#' `"Over 9 months"`/`"Under 9 months"`) are coerced to SQL `BIT` so they
#' arrive in R as `logical` vectors.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (or `tibble`, depending on [utils_db_get_query()])
#'   representing the latest extract snapshot with 17 typed columns, or `NULL`
#'   if a database connection or query error occurs:
#'   \describe{
#'     \item{`urn`}{Integer. School URN.}
#'     \item{`school_name`}{Character. Name of the establishment.}
#'     \item{`school_type`}{Character. Establishment type description.}
#'     \item{`local_authority`}{Character. Local authority name.}
#'     \item{`region`}{Character. DfE geographical region.}
#'     \item{`phase`}{Character. Phase of education (e.g., `"Primary"`, `"Secondary"`).}
#'     \item{`date_dao_approved`}{Date. Date directive Academy Order approval was granted.}
#'     \item{`date_dao_revoked`}{Date. Date dAO was revoked (`NA` if active/converted).}
#'     \item{`sponsor_match`}{Logical. `TRUE` if matched to a sponsor (`"Yes"`), `FALSE` if `"No"`.}
#'     \item{`dao_status`}{Character. Current dAO status classification. One of:
#'       `"Closed"`, `"Awaiting conversion"`,
#'       `"Special Case - Will never be issued a dAO so keep approval date blank"`,
#'       `"dAO not issued"`, `"Revoked"`, or `"Closed to become an Academy"`.}
#'     \item{`lead`}{Character. Project Lead (`PL`) assigned to the case.}
#'     \item{`new_academy_urn`}{Integer. URN of the opened academy (`NA` if not yet opened).}
#'     \item{`new_academy_open_date`}{Date. Academy opening date sourced from GIAS.}
#'     \item{`length_of_dao_months`}{Numeric. Duration to open or total length of dAO in months, rounded to 2 decimal places.}
#'     \item{`over_9_months_flag`}{Logical. `TRUE` if `"Over 9 months"`, `FALSE` if `"Under 9 months"`.}
#'     \item{`pupil_numbers`}{Integer. Pupil headcount on roll.}
#'     \item{`date_of_extract`}{Date. Snapshot extraction date.}
#'   }
#'
#' @family dAO database functions
#' @seealso [db_dao_get_all()], [db_dao_get_historical_timeline()],
#'   [sql_manager()], [utils_resolve_schema()], [log_event()]
#'
#' @export
#' @examples
#' \dontrun{
#' latest_dao <- db_dao_get_latest_extract()
#'
#' # Using an alternative connection alias
#' latest_dao_uat <- db_dao_get_latest_extract(conn_name = "dit_uat")
#' }
db_dao_get_latest_extract <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_00c")

  query <- glue::glue(
    "
    SELECT 
      TRY_CAST([URN] AS INT) AS [urn],
      [School Name] AS [school_name],
      [School Type] AS [school_type],
      [Local Authority] AS [local_authority],
      [Region] AS [region],
      [Phase] AS [phase],
      CAST([Date dAO approval granted] AS DATE) AS [date_dao_approved],
      CAST([Date dAO revoked] AS DATE) AS [date_dao_revoked],
      CASE [Sponsor Match?]
        WHEN 'Yes' THEN CAST(1 AS BIT)
        WHEN 'No'  THEN CAST(0 AS BIT)
        ELSE NULL
      END AS [sponsor_match],
      [dAO Status*] AS [dao_status],
      [PL] AS [lead],
      TRY_CAST([New Academy URN] AS INT) AS [new_academy_urn],
      CAST([New Academy Open date (from GIaS)] AS DATE) AS [new_academy_open_date],
      ROUND([How long to Open as Academy/Length of dAO (months)], 2) AS [length_of_dao_months],
      CASE [Over 9 months]
        WHEN 'Over 9 months'  THEN CAST(1 AS BIT)
        WHEN 'Under 9 months' THEN CAST(0 AS BIT)
        ELSE NULL
      END AS [over_9_months_flag],
      TRY_CAST([Pupil Numbers] AS INT) AS [pupil_numbers],
      [date_of_extract]
    FROM {schema}.[dAOList]
    WHERE [date_of_extract] = (
      SELECT MAX([date_of_extract]) 
      FROM {schema}.[dAOList]
    );
  "
  )

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch 12-month historical dAO caseload timeline
#'
#' Aggregates distinct school counts across the 12 most recent monthly extracts
#' in `dAOList`, grouped by extract date, dAO status, and the 9-month duration
#' threshold flag to support longitudinal caseload tracking.
#'
#' @details
#' Selects the `TOP (12)` distinct `[date_of_extract]` values (ordered descending)
#' from `[dAOList]` within the schema resolved by
#' `utils_resolve_schema("db_schema_00c")`, aggregates distinct `[URN]` counts,
#' coerces `[Over 9 months]` to a SQL `BIT` (`logical` in R), and returns the
#' series ordered chronologically ascending.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (or `tibble`, depending on [utils_db_get_query()])
#'   ordered chronologically by `date_of_extract` ascending with 4 typed columns,
#'   or `NULL` if a database connection or query error occurs:
#'   \describe{
#'     \item{`date_of_extract`}{Date. Snapshot extraction date (last 12 available extracts).}
#'     \item{`dao_status`}{Character. dAO status classification at the time of the extract. One of:
#'       `"Closed"`, `"Awaiting conversion"`,
#'       `"Special Case - Will never be issued a dAO so keep approval date blank"`,
#'       `"dAO not issued"`, `"Revoked"`, or `"Closed to become an Academy"`.}
#'     \item{`over_9_months_flag`}{Logical. `TRUE` if `"Over 9 months"`, `FALSE` if `"Under 9 months"`.}
#'     \item{`school_count`}{Integer. Count of distinct school `URN`s in each group.}
#'   }
#'
#' @family dAO database functions
#' @seealso [db_dao_get_all()], [db_dao_get_latest_extract()],
#'   [sql_manager()], [utils_resolve_schema()], [log_event()]
#'
#' @export
#' @examples
#' \dontrun{
#' dao_history <- db_dao_get_historical_timeline()
#' }
db_dao_get_historical_timeline <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_00c")

  query <- glue::glue(
    "
    WITH RecentMonths AS (
      SELECT DISTINCT TOP (12) [date_of_extract]
      FROM {schema}.[dAOList]
      ORDER BY [date_of_extract] DESC
    )
    SELECT 
      d.[date_of_extract],
      d.[dAO Status*] AS [dao_status],
      CASE d.[Over 9 months]
        WHEN 'Over 9 months'  THEN CAST(1 AS BIT)
        WHEN 'Under 9 months' THEN CAST(0 AS BIT)
        ELSE NULL
      END AS [over_9_months_flag],
      COUNT(DISTINCT d.[URN]) AS [school_count]
    FROM {schema}.[dAOList] d
    INNER JOIN RecentMonths r ON d.[date_of_extract] = r.[date_of_extract]
    GROUP BY d.[date_of_extract], d.[dAO Status*], d.[Over 9 months]
    ORDER BY d.[date_of_extract] ASC;
  "
  )

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}
