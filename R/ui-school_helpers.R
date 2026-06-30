#' Render School Overview Panel
#'
#' Generates an upgraded authentic GOV.UK-styled tabbed UI panel displaying summary information
#' for an individual school using the latest available Edubase snapshot.
#'
#' @param urn Character or numeric scalar. Unique Reference Number (URN) identifying the school.
#' @param id Character scalar. Optional UI container ID used to namespace tab interactions.
#' @export
school_render_overview <- function(
  urn,
  id = paste0("school_overview_", urn)
) {
  start_time <- Sys.time()
  log_event(glue::glue("Starting school_render_overview with urn: {urn}"))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )

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
        shinyGovstyle::heading_text("School Overview", size = "l"),
        shiny::HTML(
          "<p class='govuk-body'>No data available for this school.</p>"
        )
      )
    )
  }

  is_closed <- !is.na(summary_data$close_date[1])
  status_tag <- if (is_closed) {
    shiny::tags$strong(
      class = "govuk-tag govuk-tag--red",
      style = "margin-left: 15px; vertical-align: middle;",
      "Closed"
    )
  } else {
    shiny::tags$strong(
      class = "govuk-tag govuk-tag--green",
      style = "margin-left: 15px; vertical-align: middle;",
      "Open"
    )
  }

  tabs <- list()

  tabs[["School Details"]] <- list(
    "LA" = summary_data$la,
    "Region" = summary_data$region,
    "Type" = summary_data$school_type,
    "Group" = summary_data$school_type_group,
    "Opened" = if (!is.na(summary_data$open_date[1])) {
      format(as.Date(summary_data$open_date), "%d-%m-%Y")
    } else {
      "Unknown"
    }
  )

  if (is_closed) {
    tabs[["School Details"]] <- c(
      tabs[["School Details"]],
      "Closed" = format(as.Date(summary_data$close_date), "%d-%m-%Y"),
      "Reason" = summary_data$reason_closed
    )
  }

  if (!is.na(summary_data$trust_ref[1])) {
    tabs[["Trust Details"]] <- list(
      "Trust Ref" = summary_data$trust_ref,
      "Trust Name" = summary_data$trust_name
    )
  }

  fsm_formatted <- if (!is.na(summary_data$perc_fsm[1])) {
    paste0(summary_data$perc_fsm, "%")
  } else {
    "Data Not Available"
  }
  tabs[["More Details"]] <- list(
    "Phase" = summary_data$phase,
    "Religious character" = summary_data$religious_character,
    "Diocese" = summary_data$diocese_name,
    "Gender" = summary_data$gender,
    "Pupils" = if (!is.na(summary_data$pupil_number[1])) {
      prettyNum(summary_data$pupil_number, big.mark = ",")
    } else {
      "0"
    },
    "% FSM" = fsm_formatted
  )

  links <- c(
    slic_urn_url(summary_data$urn[1]),
    summary_data$school_website[1],
    ofsted_url(summary_data$urn[1]),
    gias_school_url(summary_data$urn[1])
  )
  link_names <- c("SLIC", "Website", "Ofsted Reports", "GIAS")

  if (
    !is.na(summary_data$trust_ref) &&
      summary_data$trust_ref != "" &&
      !is.na(summary_data$trust_name) &&
      summary_data$trust_name != ""
  ) {
    links <- c(links, gias_trust_url(summary_data$trust_ref[1]))
    link_names <- c(link_names, paste(summary_data$trust_name, "GIAS Trust"))
  }
  names(links) <- link_names

  link_tags <- htmltools::tags$ul(
    class = "govuk-list govuk-list--spaced",
    lapply(seq_along(links), function(i) {
      url <- links[[i]]
      label <- names(links)[i]
      htmltools::tags$li(make_shiny_link(url, label))
    })
  )
  tabs[["Important Links"]] <- link_tags

  tab_buttons <- tags$ul(
    class = "govuk-tabs__list",
    lapply(names(tabs), function(tab) {
      is_first <- (tab == names(tabs)[1])
      tags$li(
        class = if (is_first) {
          "govuk-tabs__list-item govuk-tabs__list-item--selected"
        } else {
          "govuk-tabs__list-item"
        },
        tags$a(
          class = "govuk-tabs__tab custom-tab-trigger",
          `data-tab` = tab,
          href = "javascript:void(0);",
          tab
        )
      )
    })
  )

  tab_contents <- tags$div(
    class = "govuk-tabs__panels-container",
    lapply(names(tabs), function(tab) {
      content <- tabs[[tab]]
      is_first <- (tab == names(tabs)[1])

      body <- if (tab == "Important Links") {
        content
      } else {
        shinyGovstyle::gov_summary(
          inputId = paste0(id, "_", gsub(" ", "_", tolower(tab))),
          headers = names(content),
          info = unname(content),
          border = TRUE
        )
      }

      tags$div(
        class = if (is_first) {
          "govuk-tabs__panel custom-tab-panel active"
        } else {
          "govuk-tabs__panel custom-tab-panel"
        },
        style = if (is_first) "display: block;" else "display: none;",
        `data-tab` = tab,
        body
      )
    })
  )

  tab_js <- tags$script(HTML(
    glue::glue(
      "
      (function() {{
        const root = document.getElementById('{id}');
        if (!root) return;

        root.querySelectorAll('.custom-tab-trigger').forEach(trigger => {{
          trigger.addEventListener('click', function(e) {{
            e.preventDefault();
            const targetTab = this.dataset.tab;
            const listItem = this.parentElement;

            // Strip active markers from selection array list items
            root.querySelectorAll('.govuk-tabs__list-item').forEach(li => {{
              li.classList.remove('govuk-tabs__list-item--selected');
            }});
            // Hide all active content matrix panel frames
            root.querySelectorAll('.custom-tab-panel').forEach(panel => {{
              panel.style.display = 'none';
            }});

            // Activate chosen parameters elements instantly
            listItem.classList.add('govuk-tabs__list-item--selected');
            const targetPanel = root.querySelector('.custom-tab-panel[data-tab=\"' + targetTab + '\"]');
            if (targetPanel) {{
              targetPanel.style.display = 'block';
            }}
          }});
        }});
      }})();
      "
    )
  ))

  tab_css <- tags$style(HTML(
    glue::glue(
      "
      #{id} .govuk-tabs__list {{
        list-style: none;
        padding: 0;
        margin: 0;
        border-bottom: 2px solid #b1b4b6;
        display: flex;
      }}
      #{id} .govuk-tabs__list-item {{
        margin-right: 5px;
        margin-bottom: -2px;
      }}
      #{id} .govuk-tabs__tab {{
        display: block;
        padding: 10px 20px;
        color: #1d70b8;
        text-decoration: none;
        font-weight: bold;
        background: #f3f2f1;
        border: 2px solid transparent;
        border-bottom: none;
      }}
      #{id} .govuk-tabs__list-item--selected .govuk-tabs__tab {{
        background: #ffffff;
        color: #0b0c0c;
        border-color: #b1b4b6;
        border-bottom: 2px solid #ffffff;
      }}
      #{id} .govuk-tabs__panel {{
        border: 2px solid #b1b4b6;
        border-top: none;
        padding: 20px;
        background: #ffffff;
      }}
      "
    )
  ))

  header_title_block <- shiny::tags$div(
    style = "display: flex; align-items: center; margin-bottom: 15px;",
    shinyGovstyle::heading_text(
      glue::glue("{summary_data$school_name} ({summary_data$urn})"),
      size = "l"
    ),
    status_tag
  )

  ui <- shinyGovstyle::gov_layout(
    header_title_block,
    tab_css,
    shiny::div(
      class = "govuk-tabs",
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
    class = "school-overview-wrapper govuk-!-margin-top-4",
    ui
  )
}
