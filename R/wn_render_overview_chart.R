#' Render Warning Notice Status/Type Charts + Downloads
#'
#' Renders two side-by-side Plotly bar charts:
#' - Left: x = Status, y = count (stacked by school_region if region = NULL)
#' - Right: x = Type,   y = count (stacked by school_region if region = NULL)
#'
#' If `region` is provided, data are filtered to that region and bars are not stacked.
#' Status ID 7 ("removed") is always excluded.
#'
#' Includes:
#' - Download buttons for records CSV, status summary CSV, and type summary CSV.
#' - A table of underlying records with a per-row direct wn link to the portal.
#'
#' @param region Character scalar or `NULL`. If provided, filters by `school_region`.
#'               If `NULL`, shows a stacked view by all `school_region` values.
#'
#' @return A `shiny.tag` UI fragment suitable for use in `renderUI()`.
#' @export
#'

wn_render_status_type_charts <- function(region = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting wn_render_status_type_charts with region: {region}"
  ))

  # Dependencies
  requireNamespace("DBI")
  requireNamespace("glue")
  requireNamespace("dplyr")
  requireNamespace("plotly")
  requireNamespace("htmltools")
  requireNamespace("shiny")
  requireNamespace("shinyGovstyle")
  have_DT <- requireNamespace("DT", quietly = TRUE)

  conn <- sql_manager("dit")

  region_where <- if (!is.null(region)) {
    glue::glue_sql(" AND a.school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

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
    LEFT JOIN {schema_01a}.[twn_type_conf]   t
      ON a.type_of_notice_id = t.twn_type_id
    LEFT JOIN {schema_01a}.[twn_status_conf] s
      ON a.twn_status_id     = s.twn_status_id
    WHERE a.twn_status_id <> 7
      {region_where}
  ",
    .con = conn
  )

  df <- tryCatch(
    {
      DBI::dbGetQuery(conn, sql_cmd)
    },
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching status/type data: {e$message}"
      ))
      NULL
    }
  )

  if (is.null(df) || !nrow(df)) {
    dauPortalTools::log_event("No rows returned for status/type charts.")
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
    dauPortalTools::log_event(glue::glue(
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

  # Aggregations for charts + downloads
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

  # For nicer ordering on x-axis
  status_order <- status_counts |>
    dplyr::group_by(twn_status_name) |>
    dplyr::summarise(total = sum(count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::pull(twn_status_name)

  type_order <- type_counts |>
    dplyr::group_by(twn_type_name) |>
    dplyr::summarise(total = sum(count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::pull(twn_type_name)

  # GOV.UK palette
  govuk_palette <- c(
    "#1d70b8",
    "#d4351c",
    "#00703c",
    "#ffdd00",
    "#6f72af",
    "#d53880",
    "#f47738",
    "#b58840",
    "#5694ca",
    "#28a197"
  )

  # Build Plotly charts
  if (is.null(region)) {
    p_status <- plotly::plot_ly(
      status_counts,
      x = ~twn_status_name,
      y = ~count,
      color = ~school_region,
      colors = govuk_palette,
      type = "bar",
      hovertemplate = paste(
        "<b>Status:</b> %{x}<br>",
        "<b>Region:</b> %{fullData.name}<br>",
        "<b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title = "Status",
          categoryorder = "array",
          categoryarray = status_order
        ),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = "School region"))
      )

    p_type <- plotly::plot_ly(
      type_counts,
      x = ~twn_type_name,
      y = ~count,
      color = ~school_region,
      colors = govuk_palette,
      type = "bar",
      hovertemplate = paste(
        "<b>Type:</b> %{x}<br>",
        "<b>Region:</b> %{fullData.name}<br>",
        "<b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title = "Type",
          categoryorder = "array",
          categoryarray = type_order
        ),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = "School region"))
      )
  } else {
    primary_colour <- "#1d70b8"

    p_status <- plotly::plot_ly(
      status_counts,
      x = ~twn_status_name,
      y = ~count,
      type = "bar",
      marker = list(color = primary_colour),
      hovertemplate = paste(
        "<b>Status:</b> %{x}<br>",
        "<b>Region:</b> ",
        htmltools::htmlEscape(region),
        "<br>",
        "<b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Status",
          categoryorder = "array",
          categoryarray = status_order
        ),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )

    p_type <- plotly::plot_ly(
      type_counts,
      x = ~twn_type_name,
      y = ~count,
      type = "bar",
      marker = list(color = primary_colour),
      hovertemplate = paste(
        "<b>Type:</b> %{x}<br>",
        "<b>Region:</b> ",
        htmltools::htmlEscape(region),
        "<br>",
        "<b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Type",
          categoryorder = "array",
          categoryarray = type_order
        ),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )
  }

  heading <- if (is.null(region)) {
    "Warning Notice status & type"
  } else {
    glue::glue("Warning Notice status & type — {region}")
  }

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_records_id <- paste0("wn_dl_records_csv_", rand_id)
  dl_status_id <- paste0("wn_dl_status_csv_", rand_id)
  dl_type_id <- paste0("wn_dl_type_csv_", rand_id)

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
    # Status summary CSV
    session$output[[dl_status_id]] <- shiny::downloadHandler(
      filename = function() {
        suffix <- if (is.null(region)) {
          "all_regions"
        } else {
          gsub("\\s+", "_", region)
        }
        paste0(
          "wn_status_summary_",
          suffix,
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".csv"
        )
      },
      content = function(file) {
        out <- if (is.null(region)) {
          dplyr::rename(
            status_counts,
            status = twn_status_name,
            region = school_region
          )
        } else {
          dplyr::rename(status_counts, status = twn_status_name)
        }
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )
    # Type summary CSV
    session$output[[dl_type_id]] <- shiny::downloadHandler(
      filename = function() {
        suffix <- if (is.null(region)) {
          "all_regions"
        } else {
          gsub("\\s+", "_", region)
        }
        paste0(
          "wn_type_summary_",
          suffix,
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".csv"
        )
      },
      content = function(file) {
        out <- if (is.null(region)) {
          dplyr::rename(
            type_counts,
            type = twn_type_name,
            region = school_region
          )
        } else {
          dplyr::rename(type_counts, type = twn_type_name)
        }
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )
  }

  # Build table UI
  table_heading <- htmltools::div(
    style = "margin: 1rem 0 0.25rem 0; font-weight:600;",
    "Underlying records"
  )

  table_widget <- if (have_DT) {
    DT::datatable(
      records_df |>
        dplyr::mutate(link = sprintf('%sOpen</a>', link)),
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        order = list(list(6, "desc"))
      )
    )
  } else {
    shinyGovstyle::govTable(
      inputId = paste0("wn_fallback_tbl_", rand_id),
      df = records_df,
      caption = NULL,
      caption_size = "m",
      num_col = NULL
    )
  }

  # --- Compose UI ---
  ui <- shinyGovstyle::gov_layout(
    size = "full",
    shinyGovstyle::heading_text(heading, size = "l"),
    shinyGovstyle::label_hint(
      "wn_st_hint",
      if (is.null(region)) {
        "Stacked by school region."
      } else {
        "Filtered by school region."
      }
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        htmltools::div(
          style = "margin: 0.5rem 0 0.5rem 0;",
          htmltools::tags$strong("Downloads: "),
          shiny::downloadButton(dl_records_id, "Records CSV"),
          htmltools::span(" "),
          shiny::downloadButton(dl_status_id, "Status summary CSV"),
          htmltools::span(" "),
          shiny::downloadButton(dl_type_id, "Type summary CSV")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        htmltools::div(
          style = "margin-bottom: 0.5rem; font-weight:600;",
          "By status"
        ),
        p_status
      ),
      shiny::column(
        width = 6,
        htmltools::div(
          style = "margin-bottom: 0.5rem; font-weight:600;",
          "By type"
        ),
        p_type
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        table_heading,
        table_widget
      )
    )
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
