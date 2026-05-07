#' Render Live Quality Issue Panel
#'
#' Generates a GOV.UK-styled UI panel displaying active quality issues for one or
#' more applications. Results are rendered as an interactive table with optional
#' filtering and a download option.
#'
#' @param user Character scalar or `NULL`. Optional filter for a specific user.
#' @param with_rcs Logical or `NULL`. Optional filter indicating whether the
#'   record is associated with RCS.
#' @param region Character scalar or `NULL`. Optional filter for region.
#' @param app_id Integer or `NULL`. Optional filter restricting results to a
#'   single application.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Queries the `quality_list` table for active issues (`check_active = 1`)
#'   \item Applies optional filters for application, user, region, and RCS status
#'   \item Joins application and quality metadata from `app_list` and
#'         `quality_check`
#'   \item Builds contextual links for records where applicable
#'   \item Renders results using [DT::datatable()]
#'   \item Provides a download option for exporting the dataset
#' }
#'
#' SQL queries are constructed using [glue::glue_sql()] to ensure safe
#' interpolation of input parameters.
#'
#' The UI is wrapped using [shinyGovstyle::gov_layout()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens a database connection via [sql_manager()]
#'   \item Executes SQL queries using [DBI::dbGetQuery()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A Shiny UI object. The returned object includes an attribute
#'   `"data"` containing the underlying `data.frame` used to populate the table.
#'
#' @examples
#' \dontrun{
#' quality_render_live()
#'
#' quality_render_live(app_id = 3)
#'
#' quality_render_live(user = "BSMITH7", region = "North West")
#' }
#'
#' @seealso [quality_get_data()], [db_add_portal_message()]
#'
#' @export

quality_render_live <- function(
  user = NULL,
  with_rcs = NULL,
  region = NULL,
  app_id = NULL
) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting quality_render_live with app_id: {app_id}, user: {user}, with_rcs: {with_rcs}, region: {region}"
  ))

  conf <- get_config()

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
    glue::glue_sql(" AND ql.username = '{user}'", .con = conn)
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
  ql.username AS 'User',
  ql.region AS 'Region',
  ql.with_rcs AS 'With RCS?',
  ql.date_created AS 'Date Identified',
  ql.last_checked AS 'Last Reviewed'              
  FROM {schema_01a}.[quality_list] ql
  LEFT JOIN {schema_01a}.[app_list] al
        ON ql.app_id = al.app_id
  LEFT JOIN {schema_01a}.[quality_check] qc 
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

  # Download

  table_widget <- DT::datatable(
    summary_data,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE
    )
  )

  ui <- shinyGovstyle::gov_layout(
    tabPanel(
      "Quality Issues Identified",
      shiny::fluidRow(
        htmltools::div(
          style = "margin: 0.5rem 0;",
          htmltools::tags$strong("Download all records: "),
          download_handler(
            df = summary_data,
            file_label1 = "live_quality_issues"
          )
        ),
        table_widget
      )
    )
  )

  attr(ui, "data") <- summary_data

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished quality_render_live in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
