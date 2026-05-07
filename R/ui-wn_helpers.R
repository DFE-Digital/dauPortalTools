#' Render Warning Notice Summary Panel
#'
#' Generates a GOV.UK-styled summary panel displaying key warning notice
#' metrics. The panel presents three headline figures:
#'
#' \itemize{
#'   \item Total live records (excluding removed notices)
#'   \item Records updated in the last 30 days
#'   \item Open quality issues
#' }
#'
#' @param region Character scalar or `NULL`. Optional filter restricting
#'   results to a specific region. If `NULL`, metrics are calculated across
#'   all regions.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Queries warning notice and related tables to compute summary metrics
#'   \item Excludes records with `twn_status_id = 7`
#'   \item Applies an optional region filter to relevant tables
#'   \item Calculates:
#'   \itemize{
#'     \item Total live records
#'     \item Records updated in the last 30 days
#'     \item Open quality issues (`quality_status = 0`)
#'   }
#' }
#'
#' SQL is constructed using [glue::glue_sql()] for safe parameter interpolation.
#'
#' Results are displayed using [shinyGovstyle::gov_layout()] and card-based
#' components for a GOV.UK-style presentation.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens a database connection via [sql_manager()]
#'   \item Executes SQL queries using [DBI::dbGetQuery()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A Shiny UI object containing summary metric cards.
#'
#' @examples
#' \dontrun{
#' wn_render_summary()
#'
#' wn_render_summary(region = "North West")
#' }
#'
#' @seealso [wn_render_status_type_charts()], [DBI::dbGetQuery()]
#'
#' @export

wn_render_summary <- function(region = NULL) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting wn_render_summary with region: {region}"
  ))

  conn <- sql_manager("dit")

  school_region <- if (!is.null(region)) {
    glue::glue_sql(" AND school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }
  region_filter <- if (!is.null(region)) {
    glue::glue_sql(" AND region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  sql_command <- glue::glue_sql(
    "
    SELECT
      (SELECT COUNT(twn_id)
       FROM  {schema_01a}.[twn_all_notices]
       WHERE [twn_status_id] <> 7{school_region}) AS total_live_records,
      (SELECT COUNT(t.twn_date_id)
       FROM {schema_01a}.[twn_date_tracking] t
       LEFT JOIN {schema_01a}.[twn_all_notices] a ON t.twn_id = a.twn_id
       WHERE t.updated_on >= DATEADD(DAY, -30, GETDATE()){school_region}) AS updated_records,
      (SELECT COUNT(quality_id)
       FROM {schema_01a}.[quality_list] l
       WHERE l.app_id > 0 AND l.app_id < 3 AND quality_status = 0{region_filter}) AS quality_issues
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

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished wn_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}

#' Render Warning Notice Status and Type Charts
#'
#' Generates a GOV.UK-styled Shiny UI panel displaying warning notices
#' summarised by status and type. The panel includes interactive charts,
#' summary tables, downloadable data, and a table of underlying records.
#'
#' @param region Character scalar or `NULL`. Optional filter restricting
#'   results to a specific school region. If `NULL`, all regions are included
#'   and charts are displayed using colour-coded grouping by region.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Retrieves warning notice records from the database
#'   \item Joins status and type configuration tables
#'   \item Excludes records with `twn_status_id = 7`
#'   \item Applies optional region filtering
#'   \item Cleans and prepares data for display
#'   \item Aggregates results by status and type
#'   \item Generates interactive Plotly charts using `ggplot2`
#'   \item Creates summary tables for both status and type
#'   \item Renders a table of underlying records with portal links
#'   \item Registers a download handler for exporting the dataset
#' }
#'
#' Visualisations include:
#' \itemize{
#'   \item A status distribution chart (stacked by region when unfiltered)
#'   \item A type distribution chart (horizontal orientation)
#'   \item Corresponding summary tables
#' }
#'
#' The UI is structured using `tabsetPanel()` with tabs for:
#' \itemize{
#'   \item Status Chart
#'   \item Type Chart
#'   \item Underlying Data
#' }
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens a database connection via [sql_manager()]
#'   \item Executes SQL queries using [DBI::dbGetQuery()]
#'   \item Registers Shiny download handlers within the active session
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A Shiny UI object containing charts, summary tables,
#'   download options, and underlying data.
#'
#' @examples
#' \dontrun{
#' wn_render_status_type_charts()
#'
#' wn_render_status_type_charts(region = "North West")
#' }
#'
#' @seealso [plotly::ggplotly()], [DT::datatable()]
#'
#' @export

wn_render_status_type_charts <- function(region = NULL) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting wn_render_status_type_charts with region: {region}"
  ))

  have_DT <- requireNamespace("DT", quietly = TRUE)

  conn <- sql_manager("dit")

  region_where <- if (!is.null(region)) {
    glue::glue_sql(" AND a.school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_records_id <- paste0("wn_dl_records_csv_", rand_id)

  # Download
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$output[[dl_records_id]] <- shiny::downloadHandler(
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

  sql_cmd <- glue::glue_sql(
    "
    SELECT
      a.twn_id,
      a.school_name,
      a.school_region,
      a.created_on,
      a.updated_on,
      s.twn_status_id,
      s.twn_status_name,
      t.twn_type_id,
      t.twn_type_name
    FROM {schema_01a}.[twn_all_notices] a
    LEFT JOIN {schema_01a}.[twn_type_config] t ON a.type_of_notice_id = t.twn_type_id
    LEFT JOIN {schema_01a}.[twn_status_config] s ON a.twn_status_id = s.twn_status_id
    WHERE a.twn_status_id <> 7
      {region_where}
  ",
    .con = conn
  )

  df <- tryCatch(DBI::dbGetQuery(conn, sql_cmd), error = function(e) {
    log_event(glue::glue(
      "Error fetching status/type data: {e$message}"
    ))
    NULL
  }) |>
    dplyr::mutate(
      twn_status_name = dplyr::case_when(
        twn_status_name %in%
          c(
            "Created",
            "Created 2RI case"
          ) ~ "On portal – pending regional update",
        TRUE ~ twn_status_name
      )
    )

  if (is.null(df) || !nrow(df)) {
    heading <- if (is.null(region)) {
      "Warning Notice status & type"
    } else {
      glue::glue("Warning Notice status & type — {region}")
    }
    ui_empty <- shinyGovstyle::gov_layout(
      size = "two-thirds",
      shinyGovstyle::heading_text(heading, size = "l"),
      shinyGovstyle::label_hint("wn_st_hint", "No data available to plot.")
    )
    end_time <- Sys.time()
    log_event(glue::glue(
      "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
    ))
    return(ui_empty)
  }

  # Prepare data
  df <- df |>
    dplyr::mutate(
      school_region = dplyr::coalesce(school_region, "(Unknown)"),
      twn_status_name = dplyr::coalesce(twn_status_name, "(Unknown)"),
      twn_type_name = dplyr::coalesce(twn_type_name, "(Unknown)")
    )

  records_df <- df |>
    dplyr::transmute(
      twn_id,
      school_name,
      school_region,
      status = twn_status_name,
      type = twn_type_name,
      created_on,
      updated_on,
      link = paste0(
        "https://rsconnect/rsc/warning-notice-portal/?wnid=",
        twn_id
      )
    ) |>
    dplyr::arrange(dplyr::desc(updated_on), dplyr::desc(created_on))

  records_df <- records_df |>
    dplyr::mutate(
      link = vapply(
        link,
        function(x) as.character(make_shiny_link(x, "Open")),
        character(1)
      )
    )

  # Counts
  if (is.null(region)) {
    status_counts <- df |>
      dplyr::count(twn_status_name, school_region, name = "count")
    type_counts <- df |>
      dplyr::count(twn_type_name, school_region, name = "count")
  } else {
    status_counts <- df |>
      dplyr::count(twn_status_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
    type_counts <- df |>
      dplyr::count(twn_type_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
  }

  # Summary tables
  status_summary_table <- DT::datatable(
    status_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )
  type_summary_table <- DT::datatable(
    type_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )

  # Build table UI
  table_heading <- htmltools::div(
    style = "margin: 1rem 0 0.25rem 0; font-weight:600;",
    "Underlying records"
  )
  table_widget <- DT::datatable(
    records_df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      order = list(list(6, "desc"))
    )
  )

  status_per_region <- df |>
    dplyr::summarise(
      count = dplyr::n(),
      .by = c(twn_status_name, school_region)
    ) |>
    dplyr::mutate(status_total = sum(count), .by = twn_status_name)

  type_per_region <- df |>
    dplyr::summarise(
      count = dplyr::n(),
      .by = c(twn_type_name, school_region)
    ) |>
    dplyr::mutate(type_total = sum(count), .by = twn_type_name)

  govuk_region_names <- c(
    "East Midlands",
    "East of England",
    "London",
    "North East",
    "North West",
    "South East",
    "South West",
    "West Midlands",
    "Yorkshire and the Humber"
  )

  govuk_palette <- c(
    "#F0506B",
    "#F7921E",
    "#D446A1",
    "#00AAB9",
    "#73369B",
    "#0C9144",
    "#00468C",
    "#BF1E2E",
    "#8DC53E"
  )

  p_status <- ggplot2::ggplot(
    status_per_region,
    #filter(status_per_region, school_region %in% c("West Midlands", "East of England")),
    ggplot2::aes(
      forcats::fct_reorder(twn_status_name, status_total, .desc = TRUE),
      y = count,
      fill = school_region,
      text = glue::glue("Region: {school_region}\nSchools: {count}")
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      breaks = govuk_region_names,
      values = govuk_palette
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_discrete(expand = c(0.15, 0), labels = function(x) {
      stringr::str_wrap(x, width = 10)
    }) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal()

  p_status <- plotly::ggplotly(p_status, tooltip = c("text")) |>
    plotly::layout(
      yaxis = list(fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE)
    ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      legend = list(
        x = 0.65,
        y = 0.95,
        itemclick = FALSE,
        itemdoubleclick = FALSE
      )
    ) # puts legend inside chart to save space. fine if last two columns remain small!!

  p_type <- ggplot2::ggplot(
    type_per_region,
    #filter(status_per_region, school_region %in% c("West Midlands", "East of England")),
    ggplot2::aes(
      forcats::fct_reorder(twn_type_name, type_total),
      y = count,
      fill = school_region,
      text = glue::glue("Region: {school_region}\nSchools: {count}")
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      breaks = govuk_region_names,
      values = govuk_palette
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0), labels = function(x) {
      stringr::str_wrap(x, width = 30)
    }) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()

  p_type <- plotly::ggplotly(p_type, tooltip = c("text")) |>
    plotly::layout(
      yaxis = list(fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE)
    ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      legend = list(
        x = 0.5,
        y = 0.85,
        itemclick = FALSE,
        itemdoubleclick = FALSE
      )
    )

  # --- Compose UI
  ui <- shinyGovstyle::gov_layout(
    shiny::tabsetPanel(
      id = paste0("wn_tabs_", rand_id),
      type = "tabs",
      tabPanel(
        "Status Chart",
        shiny::fluidRow(
          p_status
        ),
        shiny::fluidRow(
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Status Summary"
          ),
          status_summary_table
        )
      ),
      tabPanel(
        "Type Chart",
        shiny::fluidRow(
          p_type
        ),
        shiny::fluidRow(
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Type Summary"
          ),
          type_summary_table
        )
      ),
      tabPanel(
        "Underlying Data",
        shiny::fluidRow(
          htmltools::div(
            style = "margin: 0.5rem 0;",
            htmltools::tags$strong("Download all records: "),
            shiny::downloadButton(dl_records_id, "Records CSV")
          ),
          table_widget
        )
      )
    )
  )

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))
  return(ui)
}
