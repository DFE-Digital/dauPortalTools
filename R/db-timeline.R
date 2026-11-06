# ==============================================================================
# Database Access: Entity Timelines ([01_SSSR].[aggregate_records])
# ==============================================================================

#' Internal Query Helper for Aggregate Timeline Records
#' @noRd
#' Internal Query Helper for Aggregate Timeline Records
#' @noRd
.db_fetch_aggregate_timeline <- function(filter_col, filter_val, log_label) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Fetching timeline records for {log_label}: {filter_val}"
  ))

  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  sql_agg <- glue::glue_sql(
    "
    SELECT 
        ar.[urn],
        ar.[title],
        ar.[status],
        ar.[lead],
        u.[username] AS [lead_name],
        ar.[record_date],
        ar.[end_date],
        ar.[uri],
        ar.[type],
        ar.[trust_ref],
        ar.[region],
        ar.[la],
        ar.[diocese],
        ar.[quality_issue_count]
    FROM [Data_Insight_Team].[01_SSSR].[aggregate_records] ar
    LEFT JOIN [Data_Insight_Team].[01_SSSR].[users] u
        ON ar.[lead] = u.[user_id]
    WHERE ar.{`filter_col`} = {filter_val}
    ORDER BY ar.[record_date] DESC
    ",
    .con = conn
  )

  df_agg <- tryCatch(
    DBI::dbGetQuery(conn, sql_agg),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching aggregate_records for {log_label} {filter_val}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_agg) == 0) {
    return(data.frame())
  }

  df_agg$record_date <- as.Date(
    as.POSIXct(df_agg$record_date, tz = "UTC"),
    tz = "Europe/London"
  )

  if ("end_date" %in% names(df_agg) && !all(is.na(df_agg$end_date))) {
    df_agg$end_date <- as.Date(
      as.POSIXct(df_agg$end_date, tz = "UTC"),
      tz = "Europe/London"
    )
  }

  log_event(glue::glue(
    "Completed timeline query for {log_label}: {filter_val} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))

  df_agg
}

#' Fetch School Timeline Records
#'
#' @param urn Numeric or character. School URN.
#' @return Data frame of timeline records.
#' @export
db_get_school_timeline <- function(urn) {
  .db_fetch_aggregate_timeline(
    filter_col = "urn",
    filter_val = as.integer(urn),
    log_label = "URN"
  )
}

#' Fetch Trust Timeline Records
#'
#' @param trust_ref Character scalar. The Trust Reference (e.g., 'TR00123').
#' @return Data frame of timeline records.
#' @export
db_get_trust_timeline <- function(trust_ref) {
  .db_fetch_aggregate_timeline(
    filter_col = "trust_ref",
    filter_val = as.character(trust_ref),
    log_label = "Trust"
  )
}

#' Fetch Diocese Timeline Records
#'
#' @param diocese Character scalar. Diocese name (e.g., 'Diocese of London').
#' @return Data frame of timeline records for all schools in the specified diocese.
#' @export
db_get_diocese_timeline <- function(diocese) {
  .db_fetch_aggregate_timeline(
    filter_col = "diocese",
    filter_val = as.character(diocese),
    log_label = "Diocese"
  )
}

#' Fetch Local Authority Timeline Records
#'
#' @param la Character scalar. Local Authority name or code (e.g., 'Camden').
#' @return Data frame of timeline records for all schools in the specified LA.
#' @export
db_get_la_timeline <- function(la) {
  .db_fetch_aggregate_timeline(
    filter_col = "la",
    filter_val = as.character(la),
    log_label = "Local Authority"
  )
}
