#' Render school overview panel
#'
#' Generates a GOV.UK–styled tabbed UI layout displaying summary information
#' for an individual school, using the latest available Edubase snapshot.
#' The overview includes school details, trust details (if applicable),
#' additional metadata, and a set of useful external links.
#'
#' The rendered UI is organised into tabs, typically including:
#' \itemize{
#'   \item \strong{School Details} – Local authority, region, phase, type,
#'         opening and (if applicable) closing dates
#'   \item \strong{Trust Details} – Trust reference and name (if the school
#'         is part of a trust)
#'   \item \strong{More Details} – Demographic and structural attributes
#'         such as pupil numbers and FSM percentage
#'   \item \strong{Important Links} – Links to SLIC, the school website,
#'         Ofsted reports, GIAS, and (where applicable) trust GIAS pages
#' }
#'
#' @param urn Character string or numeric scalar representing the Unique
#'   Reference Number (URN) of the school to display.
#'
#' @return A \code{shiny.tag} UI object containing the school overview layout.
#'   If no data is available for the supplied URN, a GOV.UK–styled message
#'   indicating that no data was found is returned instead.
#'
#' @details
#' This function connects to the \code{dit} SQL Server database and queries
#' the Edubase table using the most recent \code{DateStamp} to ensure a
#' consistent snapshot view.
#'
#' Tabbed navigation is implemented using lightweight custom JavaScript and
#' CSS to provide a GOV.UK–style experience without relying on Shiny tabsets.
#' Summary content within each tab is rendered using
#' \code{shinyGovstyle::gov_summary}.
#'
#' Database access and query execution are wrapped in error handling, and
#' execution timing is logged for monitoring and performance diagnostics.
#'
#' @examples
#' \dontrun{
#' # Use within a Shiny app
#' ui <- shiny::fluidPage(
#'   shiny::uiOutput("school_overview")
#' )
#'
#' server <- function(input, output, session) {
#'   output$school_overview <- shiny::renderUI({
#'     school_render_overview(urn = "123456")
#'   })
#' }
#'
#' shiny::shinyApp(ui, server)
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[shinyGovstyle]{gov_layout}}
#'   \item \code{\link[shinyGovstyle]{gov_summary}}
#' }
#'
#' @export

school_render_overview <- function(
  urn,
  id = paste0("school_overview_", urn)
) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting school_render_overview with urn: {urn}"
  ))

  conn <- sql_manager("dit")
  schema <- utils_resolve_schema("db_schema_00c")

  sql_command <- glue::glue_sql(
    "
    SELECT
      URN AS urn,
      [EstablishmentName] AS school_name,
      [LA (name)] AS la,
      [GOR (name)] AS region,
      [TypeOfEstablishment (name)] AS school_type,
      [EstablishmentTypeGroup (name)] AS school_type_group,
      [OpenDate] AS open_date,
      [CloseDate] AS close_date,
      [ReasonEstablishmentClosed (name)] AS reason_closed,
      [PhaseOfEducation (name)] AS phase,
      [ReligiousCharacter (name)] AS religious_character,
      [Diocese (name)] AS diocese_name,
      [Gender (name)] AS gender,
      [NumberOfPupils] AS pupil_number,
      [PercentageFSM] AS perc_fsm,
      [Trusts (code)] AS trust_ref,
      [Trusts (name)] AS trust_name,
      [SchoolWebsite] AS school_website
    FROM {schema}.[Edubase]
    WHERE URN = {urn}
      AND [DateStamp] = (
        SELECT MAX(DateStamp)
        FROM {schema}.[Edubase]
      )
    ",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      log_event(glue::glue("Error fetching school overview: {e$message}"))
      NULL
    }
  )

  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(
      shiny::div(
        id = id,
        shinyGovstyle::heading_text(
          "School Overview",
          size = "l"
        ),
        shiny::HTML("<p>No data available for this school.</p>")
      )
    )
  }

  tabs <- list()

  tabs[["School Details"]] <- list(
    "LA" = summary_data$la,
    "Region" = summary_data$region,
    "Type" = summary_data$school_type,
    "Group" = summary_data$school_type_group,
    "Opened" = format(as.Date(summary_data$open_date), "%d-%m-%Y")
  )

  if (!is.na(summary_data$close_date)) {
    tabs[["School Details"]] <- c(
      tabs[["School Details"]],
      "Closed" = format(as.Date(summary_data$close_date), "%d-%m-%Y"),
      "Reason" = summary_data$reason_closed
    )
  }

  if (!is.na(summary_data$trust_ref)) {
    tabs[["Trust Details"]] <- list(
      "Trust Ref" = summary_data$trust_ref,
      "Trust Name" = summary_data$trust_name
    )
  }

  tabs[["More Details"]] <- list(
    "Phase" = summary_data$phase,
    "Religious character" = summary_data$religious_character,
    "Diocese" = summary_data$diocese_name,
    "Gender" = summary_data$gender,
    "Pupils" = prettyNum(summary_data$pupil_number, big.mark = ","),
    "% FSM" = summary_data$perc_fsm
  )

  tab_buttons <- tags$div(
    class = "custom-tabs-buttons",
    lapply(names(tabs), function(tab) {
      tags$button(
        class = if (tab == names(tabs)[1]) {
          "custom-tab-btn active"
        } else {
          "custom-tab-btn"
        },
        `data-tab` = tab,
        tab
      )
    })
  )

  tab_contents <- tags$div(
    class = "custom-tabs-contents",
    lapply(names(tabs), function(tab) {
      tags$div(
        class = if (tab == names(tabs)[1]) {
          "custom-tab-content active"
        } else {
          "custom-tab-content"
        },
        `data-tab` = tab,
        shinyGovstyle::gov_summary(
          inputId = paste0(id, "_", gsub(" ", "_", tolower(tab))),
          headers = names(tabs[[tab]]),
          info = unname(tabs[[tab]]),
          border = TRUE
        )
      )
    })
  )

  tab_js <- tags$script(HTML(
    glue::glue(
      "
      (function() {{
        const root = document.getElementById('{id}');
        if (!root) return;

        root.querySelectorAll('.custom-tab-btn').forEach(btn => {{
          btn.addEventListener('click', function() {{
            const target = this.dataset.tab;

            root.querySelectorAll('.custom-tab-btn')
              .forEach(b => b.classList.remove('active'));
            root.querySelectorAll('.custom-tab-content')
              .forEach(c => c.classList.remove('active'));

            this.classList.add('active');
            root.querySelector(
              '.custom-tab-content[data-tab=\"' + target + '\"]'
            ).classList.add('active');
          }});
        }});
      }})();
    "
    )
  ))

  tab_css <- tags$style(HTML(
    glue::glue(
      "
      #{id} .custom-tab-btn {{ background:#f3f2f1; border:1px solid #b1b4b6; padding:6px 12px; cursor:pointer; }}
      #{id} .custom-tab-btn.active {{ background:#005ea5; color:white; }}
      #{id} .custom-tab-content {{ display:none; }}
      #{id} .custom-tab-content.active {{ display:block; }}
    "
    )
  ))

  ui <- shinyGovstyle::gov_layout(
    shinyGovstyle::heading_text(
      glue::glue("{summary_data$school_name} ({summary_data$urn})"),
      size = "l"
    ),
    tab_css,
    shiny::div(
      class = "govuk-!-margin-top-3",
      tab_buttons,
      tab_contents
    ),
    tab_js
  )

  log_event(glue::glue(
    "Finished school_render_overview in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds"
  ))

  shiny::div(
    id = id,
    class = "school-overview-wrapper",
    ui
  )
}
