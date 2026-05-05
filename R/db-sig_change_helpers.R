#' Retrieve significant change records with region information
#'
#' Returns all significant change records enriched with Government Office
#' Region (GOR) information from the latest Edubase snapshot.
#'
#' @param db_get_query Function used to execute SQL queries.
#'
#' @return A data frame of significant change records with region metadata.
#' @export
db_get_sig_change_w_region <- function(
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_get_sig_change_w_region")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_sig_change_w_region")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    WITH LatestDate AS (
      SELECT MAX(DateStamp) AS MaxDate
      FROM {utils_resolve_schema('00c')}.[Edubase]
    ),
    LatestEdubase AS (
      SELECT
        e.[URN],
        e.[GOR (name)],
        e.[DateStamp]
      FROM {utils_resolve_schema('00c')}.[Edubase] e
      JOIN LatestDate d
        ON e.DateStamp = d.MaxDate
    )
    SELECT
        s.[sig_change_id],
        s.[URN],
        s.[type_of_sig_change_id],
        s.[type_of_gias_change_id],
        s.[application_type],
        s.[date_of_receipt],
        s.[date_change_takes_effect],
        s.[date_advisory_board],
        s.[admissions_variation_required],
        s.[date_admission_variation_returned],
        s.[school_meet_iss],
        s.[g7],
        s.[delivery_lead],
        s.[application_escalated],
        s.[application_escalated_to],
        s.[decision],
        s.[decision_maker_name],
        s.[decision_maker_grade],
        s.[decision_date],
        s.[gias_change_required],
        s.[date_gias_change_needed],
        s.[date_fa_needed],
        s.[all_actions_completed],
        s.[actions_completed_user_name],
        s.[actions_completed_date],
        s.[change_creation_date],
        s.[change_edit_date],
        s.[user_name_change],
        s.[user_name_edit_change],
        s.[comments],
        s.[variationtemplatesentdate],
        s.[variationonline],
        s.[giaschangedetail],
        s.[case_to_rcs],
        s.[retrospectivechange],
        s.[RSCContact],
        s.[withdrawn],
        s.[decision_comment],
        s.[action_required],
        e.[GOR (name)] AS gor_name,
        e.[DateStamp] AS edubase_datestamp
    FROM {utils_resolve_schema('db_schema_01sc')}.[tracker] s
    LEFT JOIN LatestEdubase e
      ON e.[URN] = s.[URN]
    ",
    .con = conn
  )

  db_get_query(conn, query)
}
