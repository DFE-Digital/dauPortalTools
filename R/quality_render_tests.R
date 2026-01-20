#' Render Warning Notice Summary Panel
#'
#' Creates a GOV.UK-styled summary panel displaying three key metrics:
#' - Total live TWN records
#' - Records updated in the last 30 days
#' - Open quality issues
#'
#' This UI fragment can be inserted anywhere in a Shiny app (e.g., inside
#' `uiOutput()` / `renderUI()`).
#'
#' @param region Character scalar or `NULL`. If provided, metrics are filtered
#'   to this region. If `NULL`, metrics are unfiltered. Region values must match
#'   the database column values used in the SQL.
#'
#' @return A `shiny.tag` object representing a GOV.UK-styled table suitable for
#'   use in `renderUI()`.
#'
#' @details
#' The function queries database using `DBI::dbGetQuery()` and a
#' connection from `sql_manager()`. It uses `glue_sql()` for
#' safe SQL interpolation. If the query fails, the UI will display `NA` values.
#'
#' Styling is provided by `shinyGovstyle` components (`gov_layout()`,
#' `heading_text()`, `govTable()`).
#'
#'
#' @seealso \code{\link[shinyGovstyle]{govTable}}, \code{\link[shinyGovstyle]{select_Input}}
#' @export
#'

quality_render_tests <- function(app_id = NULL, region = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting quality_render_tests with app_id: {app_id} and region: {region}"
  ))

  conn <- sql_manager("dit")

  app_id <- if (!is.null(app_id)) {
    glue::glue_sql(" AND app_id = {app_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  region <- if (!is.null(region)) {
    glue::glue_sql(" AND region = {region}", .con = conn)
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
    qcl_latest.live_issues,
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
    ORDER BY qcl.last_run DESC, qcl.quality_check_log_id DESC  -- tie-breaker
) AS qcl_latest
    WHERE check_active = 1
    {app_id}
    {region};",
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
  dauPortalTools::log_event(glue::glue(
    "Finished quality_render_tests in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
