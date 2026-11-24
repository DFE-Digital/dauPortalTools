#' Fetch Latest Ofsted Inspection for a Given URN
#'
#' @param urn Numeric or character. School URN.
#' @param publication_date Optional Date scalar to anchor to a specific inspection visit.
#' @return Single-row data frame of inspection judgements or empty data frame.
#' @export
db_ow_get_record <- function(urn, publication_date = NULL) {
  log_event("Starting db_dao_get_latest_extract", debug = TRUE)
  on.exit(log_event("Finished db_dao_get_latest_extract"), add = TRUE)
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(publication_date)) {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [urn], [school_name], [inspection_date], [publication_date], [first_publication_date], 
			 [inclusion], [curriculum_and_teaching], [achievement], [attendance_and_behaviour],
             [personal_development_and_wellbeing], [leadership_and_governance],
             [safeguarding_standards],[coc], [extract_date],
			  [created_by],[created_on],[updated_by],[updated_on]
      FROM [Data_Insight_Team].[01_SSSR].[ofsted_weekly_graded]
      WHERE [urn] = {urn_val}
      ORDER BY [first_publication_date] DESC, [inspection_date] DESC
            ",
      .con = conn
    )
  } else {
    glue::glue_sql(
      "
      SELECT TOP (1)
             [urn], [school_name], [inspection_date], [publication_date], [first_publication_date], 
			 [inclusion], [curriculum_and_teaching], [achievement], [attendance_and_behaviour],
             [personal_development_and_wellbeing], [leadership_and_governance],
             [safeguarding_standards],[coc], [extract_date],
			  [created_by],[created_on],[updated_by],[updated_on]
      FROM [Data_Insight_Team].[01_SSSR].[ofsted_weekly_graded]
      WHERE [urn] = {urn_val}
        AND ABS(DATEDIFF(day, [first_publication_date], {as.character(publication_date)})) <= 2
      ORDER BY ABS(DATEDIFF(day, [first_publication_date], {as.character(publication_date)})) ASC
      ",
      .con = conn
    )
  }

  tryCatch(
    DBI::dbGetQuery(conn, sql),
    error = function(e) {
      log_event(glue::glue(
        "Error in db_ow_get_record for URN {urn}: {e$message}"
      ))
      data.frame()
    }
  )
}

#' Fetch Ofsted Single Headline Judgement (SHJ) Record
#'
#' @param urn Numeric or character scalar.
#' @param published_date Optional Date or character scalar.
#' @return Single-row data frame or empty data frame.
#' @export
db_get_ofsted_shj_inspection <- function(urn, published_date = NULL) {
  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(published_date) || is.na(published_date)) {
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
        AND ABS(DATEDIFF(day, [published_date], {as.character(published_date)})) <= 5
      ORDER BY ABS(DATEDIFF(day, [published_date], {as.character(published_date)})) ASC
      ",
      .con = conn
    )
  }

  df <- tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) data.frame())
  if (nrow(df) > 0) {
    df$inspection_date <- as.Date(df$inspection_date)
    df$published_date <- as.Date(df$published_date)
  }
  df
}

#' Fetch Ofsted Sub-Judgements (SJ) Record (Pre-09/25 Interim Format)
#'
#' @param urn Numeric or character scalar.
#' @param published_date Optional Date or character scalar.
#' @return Single-row data frame or empty data frame.
#' @export
db_get_ofsted_sj_inspection <- function(urn, published_date = NULL) {
  conn <- sql_manager("dit")
  on.exit(
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
    add = TRUE
  )

  urn_val <- as.numeric(as.character(urn))

  sql <- if (is.null(published_date) || is.na(published_date)) {
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
        AND ABS(DATEDIFF(day, [published_date], {as.character(published_date)})) <= 5
      ORDER BY ABS(DATEDIFF(day, [published_date], {as.character(published_date)})) ASC
      ",
      .con = conn
    )
  }

  df <- tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) data.frame())
  if (nrow(df) > 0) {
    df$inspection_date <- as.Date(df$inspection_date)
    df$published_date <- as.Date(df$published_date)
  }
  df
}
