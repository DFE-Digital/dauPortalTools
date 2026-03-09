#' Get all available roles
#' @export
db_get_sig_change_w_region <- function(conn) {
  DBI::dbGetQuery(
    conn,
    "
    ;WITH LatestDate AS (
      SELECT MAX(DateStamp) AS MaxDate
      FROM [Data_Insight_Team].[00_Core].[Edubase]
    ),
    LatestEdubase AS (
      SELECT e.[URN], e.[GOR (name)], e.[DateStamp]
      FROM [Data_Insight_Team].[00_Core].[Edubase] e
      JOIN LatestDate d ON e.DateStamp = d.MaxDate
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
        e.[GOR (name)],
        e.[DateStamp]
    FROM [Data_Insight_Team].[01_sigchange].[tracker] s
    LEFT JOIN LatestEdubase e
      ON e.[URN] = s.[URN]
    "
  )
}
