#' Fetch Latest Ofsted Inspection for a Given URN
#'
#' @param urn Numeric or character. School URN.
#' @param publication_date Optional Date scalar to anchor to a specific inspection visit.
#' @return Single-row data frame of inspection judgements or empty data frame.
#' @export
db_get_latest_ofsted_inspection <- function(urn, publication_date = NULL) {
  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(publication_date)) {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [urn], [school_name], [inspection_date], [inclusion],
             [curriculum_and_teaching], [achievement], [attendance_and_behaviour],
             [personal_development_and_wellbeing], [leadership_and_governance],
             [safeguarding_standards], [ay], [extract_date], [date_created],
             [coc], [publication_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded]
      WHERE [urn] = {urn_val}
      ORDER BY [publication_date] DESC, [inspection_date] DESC
      ",
      .con = conn
    )
  } else {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [urn], [school_name], [inspection_date], [inclusion],
             [curriculum_and_teaching], [achievement], [attendance_and_behaviour],
             [personal_development_and_wellbeing], [leadership_and_governance],
             [safeguarding_standards], [ay], [extract_date], [date_created],
             [coc], [publication_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded]
      WHERE [urn] = {urn_val}
        AND ABS(DATEDIFF(day, [publication_date], {as.character(publication_date)})) <= 2
      ORDER BY ABS(DATEDIFF(day, [publication_date], {as.character(publication_date)})) ASC
      ",
      .con = conn
    )
  }

  tryCatch(
    DBI::dbGetQuery(conn, sql),
    error = function(e) {
      log_event(glue::glue(
        "Error in db_get_latest_ofsted_inspection for URN {urn}: {e$message}"
      ))
      data.frame()
    }
  )
}

#' Fetch Ofsted Single Headline Judgement (SHJ) Record
#'
#' @param urn Numeric or character scalar.
#' @param publication_date Optional Date or character scalar.
#' @return Single-row data frame or empty data frame.
#' @export
db_get_ofsted_shj_inspection <- function(urn, publication_date = NULL) {
  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(publication_date)) {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [inspection_id], [urn], [school_name], [la], [region],
             [inspection_date], [overall_effectiveness], [category_of_concern],
             [safeguarding_effective], [event_type], [school_ofsted_link],
             [school_type], [phase], [published_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_published]
      WHERE [urn] = {urn_val}
      ORDER BY [published_date] DESC, [inspection_date] DESC
      ",
      .con = conn
    )
  } else {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [inspection_id], [urn], [school_name], [la], [region],
             [inspection_date], [overall_effectiveness], [category_of_concern],
             [safeguarding_effective], [event_type], [school_ofsted_link],
             [school_type], [phase], [published_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_published]
      WHERE [urn] = {urn_val}
        AND ABS(DATEDIFF(day, [published_date], {as.character(publication_date)})) <= 2
      ORDER BY ABS(DATEDIFF(day, [published_date], {as.character(publication_date)})) ASC
      ",
      .con = conn
    )
  }

  tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) data.frame())
}

#' Fetch Ofsted Sub-Judgement (SJ) Record (Pre-09/25 Interim Format)
#'
#' @param urn Numeric or character scalar.
#' @param publication_date Optional Date or character scalar.
#' @return Single-row data frame or empty data frame.
#' @export
db_get_ofsted_sj_inspection <- function(urn, publication_date = NULL) {
  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(publication_date)) {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [inspection_id], [urn], [school_name], [la], [region],
             [inspection_date], [quality_of_education], [behaviour_and_attitudes],
             [personal_development], [effectiveness_of_leadership_and_management],
             [category_of_concern], [safeguarding_effective], [event_type],
             [school_ofsted_link], [published_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded_pre0925]
      WHERE [urn] = {urn_val}
      ORDER BY [published_date] DESC, [inspection_date] DESC
      ",
      .con = conn
    )
  } else {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [inspection_id], [urn], [school_name], [la], [region],
             [inspection_date], [quality_of_education], [behaviour_and_attitudes],
             [personal_development], [effectiveness_of_leadership_and_management],
             [category_of_concern], [safeguarding_effective], [event_type],
             [school_ofsted_link], [published_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded_pre0925]
      WHERE [urn] = {urn_val}
        AND ABS(DATEDIFF(day, [published_date], {as.character(publication_date)})) <= 2
      ORDER BY ABS(DATEDIFF(day, [published_date], {as.character(publication_date)})) ASC
      ",
      .con = conn
    )
  }

  tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) data.frame())
}
