#' Render Quality Test Results Panel
#'
#' Generates a GOV.UK-styled Shiny UI panel displaying the most recent
#' quality test results for one or more applications.
#'
#' @param app_id Integer scalar or `NULL`. Optional filter to restrict
#'   results to a single application. If `NULL`, results for all
#'   applications are returned.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Retrieves quality check definitions from `quality_check`
#'   \item Joins application context from `app_list`
#'   \item Uses `OUTER APPLY` to retrieve the most recent log entry
#'         from `quality_check_log` for each check
#'   \item Filters to active checks (`check_active = 1`)
#'   \item Applies an optional application filter
#'   \item Renders results as a `DT::datatable()`
#'   \item Provides a download option for exporting the dataset
#' }
#'
#' SQL is constructed using [glue::glue_sql()] to safely interpolate
#' parameters.
#'
#' The UI is wrapped using [shinyGovstyle::gov_layout()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens a database connection via [sql_manager()]
#'   \item Executes SQL queries using [DBI::dbGetQuery()]
#'   \item Registers a Shiny download handler when executed within a session
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A Shiny UI object containing a GOV.UK-styled layout and an
#'   interactive data table.
#'
#' @examples
#' \dontrun{
#' quality_render_tests()
#'
#' quality_render_tests(app_id = 12)
#' }
#'
#' @seealso [quality_render_live()], [DT::datatable()]
#'
#' @export

quality_render_tests <- function(app_id = NULL) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting quality_render_tests with app_id: {app_id}"
  ))

  conn <- sql_manager("dit")

  app_id <- if (!is.null(app_id)) {
    glue::glue_sql(" AND al.app_id = {app_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  sql_command <- glue::glue_sql(
    "
SELECT
    al.app_name,
    qc.quality_name,
    qc.quality_check_justification,
    qcl_latest.last_run,
    qcl_latest.new_issues,
    qcl_latest.closed_issues
FROM {schema_01a}.[quality_check] AS qc
LEFT JOIN {schema_01a}.[app_list] AS al ON al.app_id = qc.app_id
OUTER APPLY (
    SELECT TOP (1)
        qcl.quality_check_log_id,
        qcl.last_run,
        qcl.live_issues,
        qcl.closed_issues
    FROM {schema_01a}.[quality_check_log] AS qcl
    WHERE qcl.quality_check_id = qc.quality_check_id
    ORDER BY qcl.last_run DESC, qcl.quality_check_log_id DESC
    ) AS qcl_latest
    WHERE check_active = 1
    {app_id};",
    .con = conn
  )

  summary_data <- DBI::dbGetQuery(conn, sql_command)

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_tests_id <- paste0("wn_dl_records_csv_", rand_id)

  # Download
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$output[[dl_tests_id]] <- shiny::downloadHandler(
      filename = function() {
        suffix <- if (is.null(region)) {
          "all_regions"
        } else {
          gsub("\\s+", "_", region)
        }
        paste0("wn_records_", suffix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(records_df, file, row.names = FALSE, na = "")
      }
    )
  }

  table_widget <- DT::datatable(
    summary_data,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      order = list(list(6, "desc"))
    )
  )

  ui <- shinyGovstyle::gov_layout(
    tabPanel(
      "Quality Tests",
      shiny::fluidRow(
        htmltools::div(
          style = "margin: 0.5rem 0;",
          htmltools::tags$strong("Download all records: "),
          download_handler(table_widget)
        ),
        table_widget
      )
    )
  )

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished quality_render_tests in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
