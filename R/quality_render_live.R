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
#' Logging is performed using `dauPortalTools::log_event()` to record start and
#' end times.
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

quality_render_live <- function(
  user = NULL,
  with_rcs = NULL,
  region = NULL,
  app_id = NULL
) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting quality_render_live with app_id: {app_id}, user: {user}, with_rcs: {with_rcs}, region: {region}"
  ))

  url_link <- function(url_func, id, text) {
    if (is.null(id) || is.na(id) || id == "") {
      return(NA_character_)
    }

    url <- url_func(id)

    if (is.null(url) || is.na(url) || url == "") {
      return(NA_character_)
    }

    make_shiny_link(url, text)
  }

  conn <- sql_manager("dit")

  app_id <- if (!is.null(app_id)) {
    glue::glue_sql(" AND ql.app_id = {app_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  user <- if (!is.null(user)) {
    glue::glue_sql(" AND ql.user = {user}", .con = conn)
  } else {
    DBI::SQL("")
  }

  with_rcs <- if (!is.null(with_rcs)) {
    glue::glue_sql(" AND ql.with_rcs = {with_rcs}", .con = conn)
  } else {
    DBI::SQL("")
  }

  region <- if (!is.null(region)) {
    glue::glue_sql(" AND ql.region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  sql_command <- glue::glue_sql(
    "
SELECT al.app_id,
  al.app_name AS 'App Name',
  ql.record_id,
  qc.quality_description  AS 'Quality Concern',
  ql.user AS 'User',
  ql.region AS 'Region',
  ql.with_rcs AS 'With RCS?',
  ql.date_created AS 'Date Identified',
  ql.last_checked AS 'Last Reviewed'              
  FROM {`conf$database`}.{`conf$schema$db_schema_01a`}.[quality_list] ql
  LEFT JOIN {`conf$database`}.{`conf$schema$db_schema_01a`}.[app_list] al
        ON ql.app_id = al.app_id
  LEFT JOIN {`conf$database`}.{`conf$schema$db_schema_01a`}.[quality_check] qc 
        ON qc.quality_check_id = ql.error_id
    WHERE check_active = 1
    {app_id}
    {user}
    {with_rcs}
    {region};",
    .con = conn
  )

  summary_data <- DBI::dbGetQuery(conn, sql_command)

  summary_data$link <- mapply(
    function(app_id, record_id) {
      if (is.na(app_id) || is.na(record_id)) {
        return(NA_character_)
      }

      if (app_id == 1) {
        url_link(wnp_wn_url, record_id, "View Warning Notice")
      } else if (app_id == 3) {
        url_link(scp_sc_url, record_id, "View Sig Change")
      } else {
        NA_character_
      }
    },
    summary_data$app_id,
    summary_data$record_id
  )

  summary_data <- summary_data |> dplyr::select(-app_id, -record_id)

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
    )
  )

  ui <- shinyGovstyle::gov_layout(
    tabPanel(
      "Quality Issues Identified",
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
    "Finished quality_render_live in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
