#' Render Quality Test Results for an Application
#'
#' Generates a GOV.UK-styled UI panel summarising the latest quality‑check
#' results for one or more applications. The function queries SQL Server for
#' each quality check, retrieves the most recent run (via `OUTER APPLY`), and
#' displays results in a searchable, sortable `DT::datatable`.
#'
#' This function is designed to be used inside Shiny, typically within
#' `uiOutput()` / `renderUI()`. It also attaches a download handler for exporting
#' the full quality‑check dataset.
#'
#' @param app_id Integer or `NULL`.
#'   Optional filter restricting results to a single application.
#'   If `NULL`, results for all applications are returned.
#'   If provided, it is safely interpolated into the SQL using
#'   `glue::glue_sql()`.
#'
#' @return A `shiny.tag` object (UI fragment) containing:
#'   - a GOV.UK‑styled layout,
#'   - a download link, and
#'   - a `DT::datatable` widget showing quality‑check results.
#'
#' @details
#' The function:
#' - Opens a database connection via `sql_manager("dit")`.
#' - Constructs a parameterised SQL query using `glue_sql()`.
#' - Retrieves the most recent quality‑check log entry for each test using
#'   `OUTER APPLY`.
#' - Renders the results into a `DT::datatable`.
#' - Generates a unique ID for a CSV download handler bound to the user's
#'   session.
#'
#'
#' A GOV.UK‑themed layout is produced via `shinyGovstyle::gov_layout()`.
#'
#' @seealso
#'   \code{\link[shinyGovstyle]{gov_layout}},
#'   \code{\link[DT]{datatable}},
#'   \code{\link[glue]{glue_sql}}
#'
#' @examples
#' \dontrun{
#' # Render for a specific app
#' ui <- quality_render_tests(app_id = 12)
#'
#' # Render for all apps
#' ui <- quality_render_tests()
#' }
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
