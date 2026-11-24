# ==============================================================================
# File: R/db-sigchange.R
# Purpose: Database query functions for Significant Change Tracker data
#
# Functions:
#   - db_sigchange_get_all()              : Fetch all tracker records with GOR & lookup metadata
#   - db_sigchange_get_types()            : Fetch significant change type lookup table
#   - db_sigchange_get_gias_types()       : Fetch GIAS change type lookup table
#   - db_sigchange_get_gias_changes()     : Fetch linked GIAS change records
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all significant change records with region metadata
#'
#' Retrieves all significant change cases from `[01_sigchange].[tracker]`,
#' enriched with Government Office Region (GOR) metadata from the latest
#' `[00_Core].[Edubase]` snapshot and human-readable type descriptions from
#' `[sigchangetype]` and `[giaschangetype]`.
#'
#' @details
#' Queries `[tracker]`, `[sigchangetype]`, and `[giaschangetype]` within the
#' schema resolved by `utils_resolve_schema("db_schema_01sc")`, joined against
#' the latest `DateStamp` extract of `[Edubase]` within
#' `utils_resolve_schema("db_schema_00c")`.
#'
#' Business date columns stored as `datetime` in SQL Server are cast to `DATE`
#' (`Date` in R), while audit timestamps (`date_created`, `date_edited`) are
#' preserved as `POSIXct`. SQL `bit` flags arrive in R as `logical` vectors.
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
#' @return A `data.frame` with 51 typed columns, or `NULL` if a database
#'   connection or query error occurs:
#'   \describe{
#'     \item{`sig_change_id`}{Integer. Primary key identifier.}
#'     \item{`urn`}{Integer. School Unique Reference Number.}
#'     \item{`type_of_sig_change_id`}{Integer. Foreign key to `[sigchangetype]`.}
#'     \item{`type_of_sig_change`}{Character. Significant change type description.}
#'     \item{`type_of_gias_change_id`}{Integer. Foreign key to `[giaschangetype]`.}
#'     \item{`type_of_gias_change`}{Character. GIAS change type description.}
#'     \item{`application_type`}{Character. Application pathway/type.}
#'     \item{`date_of_receipt`}{Date. Date application was received.}
#'     \item{`date_change_takes_effect`}{Date. Proposed or actual implementation date.}
#'     \item{`date_advisory_board`}{Date. Date considered at Advisory Board.}
#'     \item{`admissions_variation_required`}{Logical. Whether an admissions variation is required.}
#'     \item{`date_admission_variation_returned`}{Date. Date admissions variation was returned.}
#'     \item{`school_meet_iss`}{Character. Whether school meets ISS criteria.}
#'     \item{`g7`}{Character. Grade 7 lead.}
#'     \item{`delivery_lead`}{Character. Delivery lead name.}
#'     \item{`delivery_lead_user_id`}{Integer. Canonical user ID for the delivery lead.}
#'     \item{`application_escalated`}{Logical. Whether the application was escalated.}
#'     \item{`application_escalated_to`}{Character. Escalation target/recipient.}
#'     \item{`decision`}{Character. Decision outcome.}
#'     \item{`decision_maker_name`}{Character. Name of the decision maker.}
#'     \item{`decision_maker_grade`}{Character. Grade of the decision maker.}
#'     \item{`decision_date`}{Date. Date decision was made.}
#'     \item{`gias_change_required`}{Logical. Whether a GIAS update is required.}
#'     \item{`date_gias_change_needed`}{Date. Target date for GIAS update.}
#'     \item{`date_fa_needed`}{Date. Target date for Funding Agreement.}
#'     \item{`all_actions_completed`}{Logical. Whether all post-decision actions are complete.}
#'     \item{`actions_completed_user_name`}{Character. Name of user who completed actions.}
#'     \item{`actions_completed_user_id`}{Integer. Canonical user ID who completed actions.}
#'     \item{`actions_completed_date`}{Date. Date actions were marked complete.}
#'     \item{`date_created`}{POSIXct. Record creation timestamp.}
#'     \item{`change_creation_date`}{POSIXct. Alias of `date_created` for downstream compatibility.}
#'     \item{`date_edited`}{POSIXct. Last edit timestamp.}
#'     \item{`change_edit_date`}{POSIXct. Alias of `date_edited` for downstream compatibility.}
#'     \item{`user_id_created`}{Integer. Canonical user ID of record creator.}
#'     \item{`user_id_edited`}{Integer. Canonical user ID of last editor.}
#'     \item{`user_name_change`}{Character. Legacy username of record creator.}
#'     \item{`user_name_edit_change`}{Character. Legacy username of last editor.}
#'     \item{`comments`}{Character. Case notes/comments.}
#'     \item{`variation_template_sent_date`}{Date. Date variation template was sent.}
#'     \item{`variation_online`}{Logical. Whether variation was submitted online.}
#'     \item{`gias_change_detail`}{Character. Details of required GIAS change.}
#'     \item{`case_to_rcs`}{Date. Date case was passed to RCS.}
#'     \item{`retrospective_change`}{Logical. Whether the change is retrospective.}
#'     \item{`rsc_contact`}{Character. RCS contact name.}
#'     \item{`rsc_contact_user_id`}{Integer. Canonical user ID for RCS contact.}
#'     \item{`withdrawn`}{Logical. Whether the application was withdrawn.}
#'     \item{`decision_comment`}{Character. Decision rationale/comments.}
#'     \item{`action_required`}{Logical. Whether follow-up action is required.}
#'     \item{`pass_case_to_rcs`}{Logical. Whether case should be passed to RCS.}
#'     \item{`gor_name`}{Character. Government Office Region name from latest Edubase extract.}
#'     \item{`edubase_datestamp`}{Date/POSIXct. Extract timestamp of the matched Edubase snapshot.}
#'   }
#'
#' @family sigchange database functions
#' @seealso [db_sigchange_get_types()], [db_sigchange_get_gias_types()],
#'   [db_sigchange_get_gias_changes()], [sql_manager()], [utils_resolve_schema()]
#'
#' @export
#' @examples
#' \dontrun{
#' sig_changes <- db_sigchange_get_all()
#' }
db_sigchange_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema_sc <- utils_resolve_schema("db_schema_01sc")
  schema_00c <- utils_resolve_schema("db_schema_00c")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        WITH LatestDate AS (
          SELECT MAX([DateStamp]) AS [MaxDate]
          FROM {`schema_00c`}.[Edubase]
        ),
        LatestEdubase AS (
          SELECT
            e.[URN],
            e.[GOR (name)],
            e.[DateStamp]
          FROM {`schema_00c`}.[Edubase] e
          INNER JOIN LatestDate d
            ON e.[DateStamp] = d.[MaxDate]
        )
        SELECT
          s.[sig_change_id],
          s.[URN] AS [urn],
          s.[type_of_sig_change_id],
          sct.[type_of_sig_change],
          s.[type_of_gias_change_id],
          gct.[type_of_gias_change],
          s.[application_type],
          CAST(s.[date_of_receipt] AS DATE) AS [date_of_receipt],
          CAST(s.[date_change_takes_effect] AS DATE) AS [date_change_takes_effect],
          CAST(s.[date_advisory_board] AS DATE) AS [date_advisory_board],
          s.[admissions_variation_required],
          CAST(s.[date_admission_variation_returned] AS DATE) AS [date_admission_variation_returned],
          s.[school_meet_iss],
          s.[g7],
          s.[delivery_lead],
          s.[delivery_lead_user_id],
          s.[application_escalated],
          s.[application_escalated_to],
          s.[decision],
          s.[decision_maker_name],
          s.[decision_maker_grade],
          CAST(s.[decision_date] AS DATE) AS [decision_date],
          s.[gias_change_required],
          CAST(s.[date_gias_change_needed] AS DATE) AS [date_gias_change_needed],
          CAST(s.[date_fa_needed] AS DATE) AS [date_fa_needed],
          s.[all_actions_completed],
          s.[actions_completed_user_name],
          s.[actions_completed_user_id],
          CAST(s.[actions_completed_date] AS DATE) AS [actions_completed_date],
          s.[date_created],
          s.[date_created] AS [change_creation_date],
          s.[date_edited],
          s.[date_edited] AS [change_edit_date],
          s.[user_id_created],
          s.[user_id_edited],
          s.[user_name_change],
          s.[user_name_edit_change],
          s.[comments],
          CAST(s.[variationtemplatesentdate] AS DATE) AS [variation_template_sent_date],
          s.[variationonline] AS [variation_online],
          s.[giaschangedetail] AS [gias_change_detail],
          CAST(s.[case_to_rcs] AS DATE) AS [case_to_rcs],
          s.[retrospectivechange] AS [retrospective_change],
          s.[RSCContact] AS [rsc_contact],
          s.[rsc_contact_user_id],
          s.[withdrawn],
          s.[decision_comment],
          s.[action_required],
          s.[pass_case_to_rcs],
          e.[GOR (name)] AS [gor_name],
          e.[DateStamp] AS [edubase_datestamp]
        FROM {`schema_sc`}.[tracker] s
        LEFT JOIN {`schema_sc`}.[sigchangetype] sct
          ON s.[type_of_sig_change_id] = sct.[type_of_sig_change_id]
        LEFT JOIN {`schema_sc`}.[giaschangetype] gct
          ON s.[type_of_gias_change_id] = gct.[type_of_gias_change_id]
        LEFT JOIN LatestEdubase e
          ON e.[URN] = s.[URN];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch significant change type reference lookup
#'
#' Retrieves all configured significant change categories from
#' `[01_sigchange].[sigchangetype]`, ordered alphabetically by type name.
#'
#' @details
#' Queries `[sigchangetype]` within `utils_resolve_schema("db_schema_01sc")`.
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
#' @return A `data.frame` with 6 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`type_of_sig_change_id`}{Integer. Primary key identifier.}
#'     \item{`type_of_sig_change`}{Character. Significant change category description.}
#'     \item{`theme_creation_date`}{POSIXct. Record creation timestamp.}
#'     \item{`theme_edit_date`}{POSIXct. Last edit timestamp.}
#'     \item{`user_name`}{Character. Username of creator.}
#'     \item{`user_name_edit`}{Character. Username of last editor.}
#'   }
#'
#' @family sigchange database functions
#' @seealso [db_sigchange_get_all()], [db_sigchange_get_gias_types()]
#'
#' @export
#' @examples
#' \dontrun{
#' sc_types <- db_sigchange_get_types()
#' }
db_sigchange_get_types <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema_sc <- utils_resolve_schema("db_schema_01sc")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          [type_of_sig_change_id],
          [type_of_sig_change],
          [theme_creation_date],
          [theme_edit_date],
          [user_name],
          [user_name_edit]
        FROM {`schema_sc`}.[sigchangetype]
        ORDER BY [type_of_sig_change];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch GIAS change type reference lookup
#'
#' Retrieves all configured GIAS change categories and their mapped Edubase
#' target column names from `[01_sigchange].[giaschangetype]`.
#'
#' @details
#' Queries `[giaschangetype]` within `utils_resolve_schema("db_schema_01sc")`.
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
#' @return A `data.frame` with 7 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`type_of_gias_change_id`}{Integer. Primary key identifier.}
#'     \item{`type_of_gias_change`}{Character. GIAS change category description.}
#'     \item{`edubase_column`}{Character. Corresponding column name in `[Edubase]`.}
#'     \item{`theme_creation_date`}{POSIXct. Record creation timestamp.}
#'     \item{`theme_edit_date`}{POSIXct. Last edit timestamp.}
#'     \item{`user_name`}{Character. Username of creator.}
#'     \item{`user_name_edit`}{Character. Username of last editor.}
#'   }
#'
#' @family sigchange database functions
#' @seealso [db_sigchange_get_all()], [db_sigchange_get_types()],
#'   [db_sigchange_get_gias_changes()]
#'
#' @export
#' @examples
#' \dontrun{
#' gias_types <- db_sigchange_get_gias_types()
#' }
db_sigchange_get_gias_types <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema_sc <- utils_resolve_schema("db_schema_01sc")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          [type_of_gias_change_id],
          [type_of_gias_change],
          [edubase_column],
          [theme_creation_date],
          [theme_edit_date],
          [user_name],
          [user_name_edit]
        FROM {`schema_sc`}.[giaschangetype]
        ORDER BY [type_of_gias_change];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch GIAS change records linked to significant changes
#'
#' Retrieves field-level GIAS before/after change records from
#' `[01_sigchange].[gias_changes]`, joined to `[giaschangetype]` for human-readable
#' type labels and target Edubase column mappings. Optionally filters to a single
#' `sig_change_id` or active records only.
#'
#' @details
#' Queries `[gias_changes]` joined to `[giaschangetype]` within
#' `utils_resolve_schema("db_schema_01sc")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param sig_change_id Optional integer scalar. If supplied, filters results
#'   to changes belonging to that `sig_change_id`. Defaults to `NULL` (all records).
#' @param active_only Logical scalar. If `TRUE` (default), returns only records
#'   where `active = 1`.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 14 typed columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`gias_change_id`}{Integer. Primary key identifier.}
#'     \item{`sig_change_id`}{Integer. Foreign key to `[tracker]`.}
#'     \item{`type_of_gias_change_id`}{Integer. Foreign key to `[giaschangetype]`.}
#'     \item{`type_of_gias_change`}{Character. GIAS change category description.}
#'     \item{`edubase_column`}{Character. Mapped target column name in `[Edubase]`.}
#'     \item{`old_gias_value`}{Character. Prior value recorded in GIAS.}
#'     \item{`new_gias_value`}{Character. Updated value recorded in GIAS.}
#'     \item{`active`}{Logical. Whether the GIAS change record is active.}
#'     \item{`date_added`}{POSIXct. Timestamp when record was added.}
#'     \item{`user_name`}{Character. Legacy username who added the record.}
#'     \item{`date_created`}{POSIXct. Record creation timestamp (`datetime2`).}
#'     \item{`user_id_created`}{Integer. Canonical user ID of creator.}
#'     \item{`date_edited`}{POSIXct. Last edit timestamp (`datetime2`).}
#'     \item{`user_id_edited`}{Integer. Canonical user ID of last editor.}
#'   }
#'
#' @family sigchange database functions
#' @seealso [db_sigchange_get_all()], [db_sigchange_get_gias_types()]
#'
#' @export
#' @examples
#' \dontrun{
#' # All active GIAS changes
#' gias_all <- db_sigchange_get_gias_changes()
#'
#' # GIAS changes for a specific case (including inactive)
#' gias_case <- db_sigchange_get_gias_changes(sig_change_id = 105L, active_only = FALSE)
#' }
db_sigchange_get_gias_changes <- function(
  sig_change_id = NULL,
  active_only = TRUE,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  sc_id <- NULL
  if (!is.null(sig_change_id)) {
    sc_id <- suppressWarnings(as.integer(sig_change_id))
    if (length(sc_id) != 1L || is.na(sc_id)) {
      log_event(paste0(fn_name, " failed: invalid sig_change_id"), type = 2)
      return(NULL)
    }
  }

  schema_sc <- utils_resolve_schema("db_schema_01sc")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      where_clauses <- DBI::SQL(c(
        if (!is.null(sc_id)) {
          glue::glue_sql("g.[sig_change_id] = {sc_id}", .con = conn)
        } else {
          NULL
        },
        if (isTRUE(active_only)) "g.[active] = 1" else NULL
      ))

      where_sql <- if (length(where_clauses) > 0L) {
        glue::glue_sql(
          "WHERE {DBI::SQL(paste(where_clauses, collapse = ' AND '))}",
          .con = conn
        )
      } else {
        DBI::SQL("")
      }

      query <- glue::glue_sql(
        "
        SELECT
          g.[gias_change_id],
          g.[sig_change_id],
          g.[type_of_gias_change_id],
          gct.[type_of_gias_change],
          gct.[edubase_column],
          g.[old_gias_value],
          g.[new_gias_value],
          g.[active],
          g.[date_added],
          g.[user_name],
          g.[date_created],
          g.[user_id_created],
          g.[date_edited],
          g.[user_id_edited]
        FROM {`schema_sc`}.[gias_changes] g
        LEFT JOIN {`schema_sc`}.[giaschangetype] gct
          ON g.[type_of_gias_change_id] = gct.[type_of_gias_change_id]
        {where_sql}
        ORDER BY g.[sig_change_id], g.[gias_change_id];
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}
