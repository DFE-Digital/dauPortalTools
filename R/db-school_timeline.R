#' Fetch School Timeline Records with Optional Ofsted Inspection Data
#'
#' @description
#' Queries `aggregate_records` for a given URN ordered chronologically. If any
#' event records have `lead == 'Ofsted'`, it queries `ofsted_weekly_graded` and joins
#' the inspection details where `record_date = publication_date`.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
#' @param conn An active DBI database connection. If `NULL`, uses `sql_manager("dit")`.
#'
#' @return A data frame containing ordered timeline records and joined Ofsted judgements where available.
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_school_timeline(urn = 123456)
#' }
db_get_school_timeline <- function(urn, conn = NULL) {
  start_time <- Sys.time()
  log_event(glue::glue("Fetching timeline records for URN: {urn}"))

  should_close <- FALSE
  if (is.null(conn)) {
    conn <- sql_manager("dit")
    should_close <- TRUE
  }
  if (should_close) {
    on.exit(
      tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
      add = TRUE
    )
  }

  # 1. Fetch aggregate records for this school
  sql_agg <- glue::glue_sql(
    "
    SELECT [urn],
           [category],
           [status],
           [lead],
           [record_date],
           [uri],
           [type]
    FROM [Data_Insight_Team].[01_AIDT].[aggregate_records]
    WHERE [urn] = {urn}
    ORDER BY [record_date] ASC
    ",
    .con = conn
  )

  df_agg <- tryCatch(
    DBI::dbGetQuery(conn, sql_agg),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching aggregate_records for URN {urn}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_agg) == 0) {
    return(data.frame())
  }

  df_agg$record_date <- suppressWarnings(as.Date(df_agg$record_date))

  # 2. Check for Ofsted records
  has_ofsted <- any(tolower(trimws(df_agg$lead)) == "ofsted", na.rm = TRUE)

  if (!has_ofsted) {
    return(df_agg)
  }

  # 3. Pull Ofsted weekly graded details for this school
  sql_ofsted <- glue::glue_sql(
    "
    SELECT TOP (1000)
           [urn],
           [school_name],
           [inspection_date],
           [inclusion],
           [curriculum_and_teaching],
           [achievement],
           [attendance_and_behaviour],
           [personal_development_and_wellbeing],
           [leadership_and_governance],
           [safeguarding_standards],
           [ay],
           [extract_date],
           [date_created],
           [coc],
           [publication_date]
    FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded]
    WHERE [urn] = {urn}
    ",
    .con = conn
  )

  df_ofsted <- tryCatch(
    DBI::dbGetQuery(conn, sql_ofsted),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching ofsted_weekly_graded for URN {urn}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_ofsted) > 0) {
    df_ofsted$publication_date <- suppressWarnings(as.Date(
      df_ofsted$publication_date
    ))
    df_ofsted$inspection_date <- suppressWarnings(as.Date(
      df_ofsted$inspection_date
    ))

    # Match urn = urn and record_date = publication_date
    df_merged <- dplyr::left_join(
      df_agg,
      df_ofsted,
      by = c("urn" = "urn", "record_date" = "publication_date")
    )
  } else {
    df_merged <- df_agg
  }

  log_event(glue::glue(
    "Completed db_get_school_timeline for URN: {urn} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))

  df_merged
}
