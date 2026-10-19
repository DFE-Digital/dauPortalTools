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
