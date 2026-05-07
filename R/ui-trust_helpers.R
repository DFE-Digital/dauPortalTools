#' Render trust overview panel
#'
#' Generates a GOV.UK–styled UI panel summarising trust-level information using
#' the latest available monthly snapshot from the Chains dataset. The panel is
#' presented as a tabbed layout showing trust details and a list of schools
#' belonging to the trust.
#'
#' The UI contains two tabs:
#' \itemize{
#'   \item \strong{Trust Details} – Trust ID, name, type, region, and number of
#'         schools in the trust
#'   \item \strong{Schools in Trust} – URN, academy name, local authority,
#'         region, and date joined trust
#' }
#'
#' @param urn Optional character or numeric scalar. If supplied, the function
#'   resolves the corresponding \code{Trust_ID} from the latest Chains snapshot.
#'   Ignored if \code{trust_id} is provided.
#' @param trust_id Optional character or numeric scalar identifying the trust.
#'   If supplied, no URN lookup is performed.
#'
#' @return A \code{shiny.tag} UI fragment suitable for use inside \code{renderUI()}
#'   or directly in a Shiny UI definition.
#'
#' @details
#' The function always uses the most recent \code{DateStamp} available in the
#' Chains dataset to ensure consistency across trust details and associated
#' schools.
#'
#' If neither \code{urn} nor \code{trust_id} is provided, or if the trust cannot
#' be resolved in the latest snapshot, a GOV.UK–styled fallback UI is returned
#' explaining the issue to the user.
#'
#' All database access is wrapped in error handling with log messages generated
#' for monitoring and diagnostics.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[shinyGovstyle]{gov_layout}}
#'   \item \code{\link[shinyGovstyle]{govTabs}}
#' }
#'
#' @export

trust_render_overview <- function(urn = NULL, trust_id = NULL) {
  start_time <- Sys.time()

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  safe_na <- function(x) ifelse(is.na(x) | length(x) == 0, NA, x)

  log_event(glue::glue(
    "Starting trust_render_overview urn={urn %||% 'NULL'}, trust_id={trust_id %||% 'NULL'}"
  ))

  conn <- sql_manager("dit")

  # Resolve trust_id from URN if not provided
  if (is.null(trust_id)) {
    if (is.null(urn)) {
      log_event(
        "No URN or Trust_ID provided; returning fallback UI"
      )
      return(
        shinyGovstyle::gov_layout(
          size = "two-thirds",
          shinyGovstyle::heading_text("Trust Overview", size = "l"),
          shinyGovstyle::label_hint(
            "trust_overview_label",
            "Missing URN / Trust ID"
          ),
          shiny::p("Please supply a URN or a Trust_ID.")
        )
      )
    }

    trust_id_q <- glue::glue_sql(
      "
      SELECT TOP 1 [Trust_ID]
      FROM {conf$database}.{conf$db_schema_00c}.[Chains]
      WHERE [URN] = {urn}
        AND [DateStamp] = (SELECT MAX([DateStamp]) FROM {`conf$database`}.{`conf$schemas$db_schema_00c`}.[Chains])
    ",
      .con = conn
    )

    trust_row <- tryCatch(
      DBI::dbGetQuery(conn, trust_id_q),
      error = function(e) {
        log_event(glue::glue(
          "Error resolving Trust_ID by URN: {e$message}"
        ))
        data.frame()
      }
    )

    if (nrow(trust_row) == 0 || is.na(trust_row$Trust_ID[1])) {
      log_event("No Trust_ID found for URN in latest snapshot")
      return(
        shinyGovstyle::gov_layout(
          size = "two-thirds",
          shinyGovstyle::heading_text(
            glue::glue("Trust Overview for URN {urn}"),
            size = "l"
          ),
          shinyGovstyle::label_hint("trust_overview_label", "Latest snapshot"),
          shiny::p("No trust found for this school in the latest snapshot.")
        )
      )
    }
    trust_id <- trust_row$Trust_ID[1]
  }

  # Trust details
  trust_details_q <- glue::glue_sql(
    "
    SELECT TOP 1
      [Trust_ID],
      [Trust_Name],
      [Trust_Type],
      [Trust_Region],
      [Number_In_Trust],
      [DateStamp]
    FROM {`conf$database`}.{`conf$schemas$db_schema_00c`}.[Chains]
    WHERE [Trust_ID] = {trust_id}
      AND [DateStamp] = (SELECT MAX([DateStamp]) FROM {`conf$database`}.{`conf$schemas$db_schema_00c`}.[Chains])
  ",
    .con = conn
  )

  trust_details <- tryCatch(
    DBI::dbGetQuery(conn, trust_details_q),
    error = function(e) {
      log_event(glue::glue(
        "Error fetching trust details: {e$message}"
      ))
      data.frame()
    }
  )

  if (nrow(trust_details) == 0) {
    log_event("Trust details not found in latest snapshot")
    return(
      shinyGovstyle::gov_layout(
        size = "two-thirds",
        shinyGovstyle::heading_text(
          glue::glue("Trust Overview ({trust_id})"),
          size = "l"
        ),
        shinyGovstyle::label_hint("trust_overview_label", "Latest snapshot"),
        shiny::p("Trust not found in the latest snapshot.")
      )
    )
  }

  # Schools in trust
  schools_q <- glue::glue_sql(
    "
    SELECT
      [URN],
      [Academy_Name],
      [Local_Authority],
      [Region],
      [Date_Joined_Trust]
    FROM {`conf$database`}.{`conf$schemas$db_schema_00c`}.[Chains]
    WHERE [Trust_ID] = {trust_id}
      AND [DateStamp] = (SELECT MAX([DateStamp]) FROM {`conf$database`}.{`conf$schemas$db_schema_00c`}.[Chains])
    ORDER BY [Academy_Name]
  ",
    .con = conn
  )

  schools_df <- tryCatch(DBI::dbGetQuery(conn, schools_q), error = function(e) {
    log_event(glue::glue(
      "Error fetching schools for trust: {e$message}"
    ))
    data.frame()
  })

  # Build UI df

  # Trust Details tab
  td <- trust_details[1, , drop = FALSE]
  trust_rows <- c(
    Trust_ID = safe_na(td$Trust_ID),
    Trust_Name = safe_na(td$Trust_Name),
    Trust_Type = safe_na(td$Trust_Type),
    Trust_Region = safe_na(td$Trust_Region),
    Number_In_Trust = safe_na(td$Number_In_Trust)
  )

  trust_details_html <- shiny::HTML(paste0(
    "<ul>",
    paste0(
      "<li><strong>",
      htmltools::htmlEscape(names(trust_rows)),
      ":</strong> ",
      htmltools::htmlEscape(ifelse(
        is.na(trust_rows),
        "",
        as.character(trust_rows)
      )),
      "</li>",
      collapse = ""
    ),
    "</ul>"
  ))

  # Schools in Trust tab table
  schools_table_html <- if (nrow(schools_df) == 0) {
    shiny::HTML(
      "<p>No schools found for this trust in the latest snapshot.</p>"
    )
  } else {
    col_map <- c(
      URN = "URN",
      Academy_Name = "Academy name",
      Local_Authority = "LA",
      Region = "Region",
      Date_Joined_Trust = "Date joined trust"
    )
    render_df <- schools_df[, names(col_map), drop = FALSE]
    render_df[] <- lapply(render_df, function(x) {
      if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
    })

    rows_html <- paste0(
      apply(render_df, 1, function(r) {
        paste0(
          "<tr class='govuk-table__row'>",
          paste0(
            "<td class='govuk-table__cell'>",
            htmltools::htmlEscape(r),
            "</td>",
            collapse = ""
          ),
          "</tr>"
        )
      }),
      collapse = ""
    )

    shiny::HTML(paste0(
      "<table class='govuk-table'>",
      "<thead class='govuk-table__head'><tr class='govuk-table__row'>",
      paste0(
        "<th scope='col' class='govuk-table__header'>",
        htmltools::htmlEscape(unname(col_map)),
        "</th>",
        collapse = ""
      ),
      "</tr></thead>",
      "<tbody class='govuk-table__body'>",
      rows_html,
      "</tbody></table>"
    ))
  }

  # Heading + tabs
  snapshot_txt <- tryCatch(as.character(td$DateStamp[1]), error = function(e) {
    "Latest snapshot"
  })
  heading <- glue::glue("{td$Trust_Name} ({td$Trust_ID})")

  gov_tabs_ui <- shinyGovstyle::govTabs(
    inputId = "trust_summary_tabs",
    tabs = list(
      shiny::tabPanel(title = "Trust Details", trust_details_html),
      shiny::tabPanel(title = "Schools in Trust", schools_table_html)
    )
  )

  ui <- shinyGovstyle::gov_layout(
    size = "two-thirds",
    shinyGovstyle::heading_text(heading, size = "l"),
    shinyGovstyle::label_hint(
      "trust_summary_label",
      glue::glue("Snapshot: {snapshot_txt}")
    ),
    gov_tabs_ui
  )

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished trust_render_overview in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
