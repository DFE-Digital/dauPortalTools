#' Render Significant Change Summary Panel
#'
#' Generates a GOV.UK-styled Shiny UI panel displaying key metrics for
#' significant change records. The panel presents three headline figures:
#'
#' \itemize{
#'   \item Total live records
#'   \item Records updated in the last 30 days
#'   \item Open quality issues
#' }
#'
#' @param user Character scalar or `NULL`. Optional filter restricting results
#'   to cases where the user is either `delivery_lead` or `RSCContact`.
#'   If `NULL`, no user-level filtering is applied.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Queries the tracker and quality tables to compute summary metrics
#'   \item Applies optional filtering for user ownership
#'   \item Calculates:
#'   \itemize{
#'     \item Total live records (`all_actions_completed <> 1`)
#'     \item Records updated within the last 30 days
#'     \item Open quality issues (`quality_status = 0`)
#'   }
#' }
#'
#' SQL is constructed using [glue::glue_sql()] to safely interpolate inputs.
#'
#' Results are rendered as a GOV.UK-styled layout using
#' [shinyGovstyle::gov_layout()] and card components.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens a database connection via [sql_manager()]
#'   \item Executes SQL queries using [DBI::dbGetQuery()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A Shiny UI object containing summary cards.
#'
#' @examples
#' \dontrun{
#' sc_render_summary()
#'
#' sc_render_summary(user = "BSMITH7")
#' }
#'
#' @seealso [sc_render_status_type_charts()], [DBI::dbGetQuery()]
#'
#' @export

sc_render_summary <- function(user = NULL) {
  start_time <- Sys.time()

  log_event(glue::glue(
    "Starting sc_render_summary"
  ))

  app_id <- conf$app_details$app_id
  db_schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)
  db_schema_01sc <- DBI::SQL(conf$schemas$db_schema_01sc)

  conn <- sql_manager("dit")

  user_query <- if (!is.null(user)) {
    glue::glue_sql(
      " AND ([delivery_lead] = '{user}' OR [RSCContact] = '{user}')",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  sql_command <- glue::glue_sql(
    "
    SELECT
      (SELECT COUNT(t.sig_change_id)
       FROM {db_schema_01sc}.[tracker] t
       WHERE [all_actions_completed] <> 1 {user_query}) AS total_live_records,
      (SELECT COUNT(t.sig_change_id)
       FROM {db_schema_01sc}.[tracker] t
       WHERE change_edit_date >= DATEADD(DAY, -30, GETDATE()){user_query}) AS updated_records,
      (SELECT COUNT(quality_id)
       FROM {db_schema_01a}.[quality_list] l
       RIGHT JOIN {db_schema_01sc}.[tracker] t on l.record_id = t.sig_change_id
       WHERE l.app_id = {app_id} AND l.quality_status = 0{user_query}) AS quality_issues
  ",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      data.frame(
        total_live_records = NA_integer_,
        updated_records = NA_integer_,
        quality_issues = NA_integer_
      )
    }
  )

  total_live <- suppressWarnings(as.integer(summary_data$total_live_records[1]))
  updated_30d <- suppressWarnings(as.integer(summary_data$updated_records[1]))
  qual_issues <- suppressWarnings(as.integer(summary_data$quality_issues[1]))

  fmt <- function(x) {
    ifelse(is.na(x), "—", prettyNum(x, big.mark = ",", preserve.width = "none"))
  }

  ui <- shinyGovstyle::gov_layout(
    layout_column_wrap(
      width = 1 / 3,
      card(
        card_header("Total Live Records"),
        tags$h2(fmt(total_live), class = "govuk-heading-m")
      ),
      card(
        card_header("Updates This Month"),
        tags$h2(fmt(updated_30d), class = "govuk-heading-m")
      ),
      card(
        card_header("Quality Issues"),
        tags$h2(fmt(qual_issues), class = "govuk-heading-m")
      )
    )
  )

  DBI::dbDisconnect(conn)

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished sc_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
