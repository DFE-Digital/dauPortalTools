#' Internal Helper: Match Timeline Events with Ofsted Inspections
#' @noRd
.link_timeline_ofsted <- function(df_agg, conn) {
  has_ofsted <- any(tolower(trimws(df_agg$lead)) == "ofsted", na.rm = TRUE)
  if (!has_ofsted) {
    return(df_agg)
  }

  distinct_urns <- unique(stats::na.omit(df_agg$urn))
  if (length(distinct_urns) == 0) {
    return(df_agg)
  }

  sql_ofsted <- glue::glue_sql(
    "
    SELECT [urn],
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
    WHERE [urn] IN ({distinct_urns*})
    ",
    .con = conn
  )

  df_ofsted <- tryCatch(
    DBI::dbGetQuery(conn, sql_ofsted),
    error = function(e) {
      log_event(glue::glue("Error fetching ofsted_weekly_graded: {e$message}"))
      data.frame()
    }
  )

  if (nrow(df_ofsted) == 0) {
    return(df_agg)
  }

  df_ofsted$publication_date <- as.Date(df_ofsted$publication_date)
  df_ofsted$inspection_date <- as.Date(df_ofsted$inspection_date)

  ofsted_rows <- df_agg |> dplyr::filter(tolower(trimws(lead)) == "ofsted")
  other_rows <- df_agg |>
    dplyr::filter(tolower(trimws(lead)) != "ofsted" | is.na(lead))

  if (nrow(ofsted_rows) > 0) {
    matched_ofsted <- ofsted_rows |>
      dplyr::left_join(df_ofsted, by = "urn") |>
      dplyr::mutate(
        date_diff = abs(as.numeric(difftime(
          record_date,
          publication_date,
          units = "days"
        )))
      ) |>
      dplyr::filter(is.na(date_diff) | date_diff <= 2) |>
      dplyr::group_by(urn, record_date) |>
      dplyr::slice_min(order_by = date_diff, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(-date_diff)

    dplyr::bind_rows(matched_ofsted, other_rows) |>
      dplyr::arrange(record_date)
  } else {
    df_agg
  }
}

#' Fetch School Timeline Records
#'
#' @param urn Numeric or character. School URN.
#' @return Data frame of timeline records with joined Ofsted details.
#' @export
db_get_school_timeline <- function(urn) {
  start_time <- Sys.time()
  log_event(glue::glue("Fetching timeline records for URN: {urn}"))

  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  sql_agg <- glue::glue_sql(
    "
    SELECT [urn], [category], [status], [lead], [record_date],
           [uri], [type], [trust_ref], [region], [la], [diocese]
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

  df_agg$lead <- gsub("(?i)ofsed", "Ofsted", df_agg$lead, perl = TRUE)
  df_agg$category <- gsub("(?i)ofsed", "Ofsted", df_agg$category, perl = TRUE)
  df_agg$record_date <- as.Date(
    as.POSIXct(df_agg$record_date, tz = "UTC"),
    tz = "Europe/London"
  )

  res <- .link_timeline_ofsted(df_agg, conn)
  log_event(glue::glue(
    "Completed db_get_school_timeline for URN: {urn} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  res
}

#' Fetch Trust Timeline Records
#'
#' @param trust_ref Character scalar. The Trust Reference (e.g., 'TR00123').
#' @return Data frame of timeline records for all schools in the specified trust.
#' @export
db_get_trust_timeline <- function(trust_ref) {
  start_time <- Sys.time()
  log_event(glue::glue("Fetching timeline records for Trust: {trust_ref}"))

  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  sql_agg <- glue::glue_sql(
    "
    SELECT [urn], [category], [status], [lead], [record_date],
           [uri], [type], [trust_ref], [region], [la], [diocese]
    FROM [Data_Insight_Team].[01_AIDT].[aggregate_records]
    WHERE [trust_ref] = {trust_ref}
    ORDER BY [record_date] ASC
    ",
    .con = conn
  )

  df_agg <- tryCatch(
    DBI::dbGetQuery(conn, sql_agg),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching aggregate_records for Trust {trust_ref}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_agg) == 0) {
    return(data.frame())
  }

  df_agg$lead <- gsub("(?i)ofsed", "Ofsted", df_agg$lead, perl = TRUE)
  df_agg$category <- gsub("(?i)ofsed", "Ofsted", df_agg$category, perl = TRUE)
  df_agg$record_date <- as.Date(
    as.POSIXct(df_agg$record_date, tz = "UTC"),
    tz = "Europe/London"
  )

  res <- .link_timeline_ofsted(df_agg, conn)
  log_event(glue::glue(
    "Completed db_get_trust_timeline for Trust: {trust_ref} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  res
}

#' Fetch Diocese Timeline Records
#'
#' @param diocese Character scalar. Diocese name (e.g., 'Diocese of London').
#' @return Data frame of timeline records for all schools in the specified diocese.
#' @export
db_get_diocese_timeline <- function(diocese) {
  start_time <- Sys.time()
  log_event(glue::glue("Fetching timeline records for Diocese: {diocese}"))

  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  sql_agg <- glue::glue_sql(
    "
    SELECT [urn], [category], [status], [lead], [record_date],
           [uri], [type], [trust_ref], [region], [la], [diocese]
    FROM [Data_Insight_Team].[01_AIDT].[aggregate_records]
    WHERE [diocese] = {diocese}
    ORDER BY [record_date] ASC
    ",
    .con = conn
  )

  df_agg <- tryCatch(
    DBI::dbGetQuery(conn, sql_agg),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching aggregate_records for Diocese {diocese}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_agg) == 0) {
    return(data.frame())
  }

  df_agg$lead <- gsub("(?i)ofsed", "Ofsted", df_agg$lead, perl = TRUE)
  df_agg$category <- gsub("(?i)ofsed", "Ofsted", df_agg$category, perl = TRUE)
  df_agg$record_date <- as.Date(
    as.POSIXct(df_agg$record_date, tz = "UTC"),
    tz = "Europe/London"
  )

  res <- .link_timeline_ofsted(df_agg, conn)
  log_event(glue::glue(
    "Completed db_get_diocese_timeline for Diocese: {diocese} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  res
}

#' Fetch Local Authority Timeline Records
#'
#' @param la Character scalar. Local Authority name (e.g., 'Camden').
#' @return Data frame of timeline records for all schools in the specified LA.
#' @export
db_get_la_timeline <- function(la) {
  start_time <- Sys.time()
  log_event(glue::glue("Fetching timeline records for Local Authority: {la}"))

  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  sql_agg <- glue::glue_sql(
    "
    SELECT [urn], [category], [status], [lead], [record_date],
           [uri], [type], [trust_ref], [region], [la], [diocese]
    FROM [Data_Insight_Team].[01_AIDT].[aggregate_records]
    WHERE [la] = {la}
    ORDER BY [record_date] ASC
    ",
    .con = conn
  )

  df_agg <- tryCatch(
    DBI::dbGetQuery(conn, sql_agg),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching aggregate_records for LA {la}: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(df_agg) == 0) {
    return(data.frame())
  }

  df_agg$lead <- gsub("(?i)ofsed", "Ofsted", df_agg$lead, perl = TRUE)
  df_agg$category <- gsub("(?i)ofsed", "Ofsted", df_agg$category, perl = TRUE)
  df_agg$record_date <- as.Date(
    as.POSIXct(df_agg$record_date, tz = "UTC"),
    tz = "Europe/London"
  )

  res <- .link_timeline_ofsted(df_agg, conn)
  log_event(glue::glue(
    "Completed db_get_la_timeline for LA: {la} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  res
}
