#' Quality Table Module Server (Dashboard View)
#'
#' Orchestrates fetching and filtering the full suite of live quality issues
#' across the application dashboard.
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier.
#' @export
server_quality_wrapper <- function(id, app_id) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      TRUE,
      {
        raw_ui <- quality_render_live(app_id = app_id)
        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- shiny::reactive({
      shiny::req(df())
      x <- df()

      if (
        !is.null(input$region_filter) &&
          input$region_filter != "All" &&
          "Region" %in% names(x)
      ) {
        x <- x[x$Region == input$region_filter, , drop = FALSE]
      }

      if (
        !is.null(input$with_rcs_filter) &&
          input$with_rcs_filter != "All" &&
          "With RCS?" %in% names(x)
      ) {
        x <- x[
          x$`With RCS?` == as.numeric(input$with_rcs_filter),
          ,
          drop = FALSE
        ]
      }

      if ("Description" %in% names(x)) {
        names(x)[names(x) == "Description"] <- "Steps to Fix"
      }

      for (col in c("Date Identified", "Last Reviewed")) {
        if (col %in% names(x)) {
          parsed <- tryCatch(as.Date(x[[col]]), error = function(e) NULL)
          if (!is.null(parsed)) {
            formatted <- format(parsed, "%d/%m/%Y")
            formatted[is.na(parsed)] <- ""
            x[[col]] <- formatted
          }
        }
      }

      x
    })

    output$filtered_table <- DT::renderDT({
      shiny::req(filtered())
      DT::datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
  })
}

#' Quality Issues Table Server for a Specific Record
#'
#' Module server that automatically handles app contexts, fetches quality issues
#' for a single specific record ID, and renders a clean datatable.
#'
#' @param id Character scalar. Shiny module ID.
#' @param record_id Reactive expression or numeric scalar. The active record ID to filter by.
#' @export
server_quality_record_table <- function(id, record_id) {
  shiny::moduleServer(id, function(input, output, session) {
    record_issues <- shiny::reactive({
      r_id <- if (shiny::is.reactive(record_id)) record_id() else record_id
      shiny::req(r_id)

      x <- quality_get_data(record = r_id)
      shiny::req(x)

      if ("Description" %in% names(x)) {
        names(x)[names(x) == "Description"] <- "Steps to Fix"
      }

      for (col in c("Date Identified", "Last Reviewed")) {
        if (col %in% names(x)) {
          parsed <- tryCatch(as.Date(x[[col]]), error = function(e) NULL)
          if (!is.null(parsed)) {
            formatted <- format(parsed, "%d/%m/%Y")
            formatted[is.na(parsed)] <- ""
            x[[col]] <- formatted
          }
        }
      }

      x
    })

    output$quality_table <- DT::renderDT({
      shiny::req(record_issues())

      DT::datatable(
        record_issues(),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 't',
          autoWidth = TRUE
        )
      )
    })
  })
}

#' Quality Table Module Server (Dashboard View)
#'
#' Orchestrates fetching, reactive in-memory filtering, and cross-application
#' deep-linking layout configuration for the global quality issues view.
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier used for contextual initialization.
#' @export
server_quality_wrapper <- function(id, app_id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Hold data separately from UI
    df <- shiny::reactiveVal(NULL)

    # Load data dynamically using the clean new database function
    shiny::observeEvent(
      TRUE,
      {
        issues_df <- quality_get_dashboard_records(
          app_id = app_id
        )
        df(issues_df)
      },
      once = TRUE
    )

    filtered <- shiny::reactive({
      shiny::req(df())
      x <- df()

      # 1. Apply UI Filters instantly in local R session memory
      if (
        !is.null(input$region_filter) &&
          input$region_filter != "All" &&
          "region" %in% names(x)
      ) {
        x <- x[x$region == input$region_filter, , drop = FALSE]
      }

      if (
        !is.null(input$with_rcs_filter) &&
          input$with_rcs_filter != "All" &&
          "with_rcs" %in% names(x)
      ) {
        x <- x[x$with_rcs == as.numeric(input$with_rcs_filter), , drop = FALSE]
      }

      shiny::req(nrow(x) > 0)

      # 2. Map cross-application URLs dynamically based on target app_id properties
      links <- sapply(seq_len(nrow(x)), function(i) {
        row_app <- x$app_id[i]
        row_rec <- x$record_id[i]

        if (is.na(row_app) || is.na(row_rec)) {
          return("#")
        }

        if (row_app == 1) {
          return(wnp_wn_url(row_rec)) # Warning Notices Router
        } else if (row_app == 3) {
          return(scp_sc_url(row_rec)) # Significant Changes Router
        }

        return("#")
      })

      # 3. Splice target anchor links and copy-to-clipboard buttons together
      x$Actions <- sprintf(
        "<div class='govuk-button-group' style='margin:0; display:flex; gap:12px; align-items:center;'>
           <a href='%s' class='govuk-link' style='font-weight:700; white-space:nowrap;'>View Record</a>
           <button type='button' class='govuk-button govuk-button--secondary dt-copy-btn' data-copy-text='%s' style='margin:0; padding:4px 8px; font-size:13px; white-space:nowrap;'>Copy Link</button>
         </div>",
        links,
        links
      )

      # 4. Strip out relational attributes and return clean, explicitly requested columns
      data.frame(
        `URN` = x$urn,
        `Region` = x$region,
        `Quality Issue` = x$quality_issue,
        `Actions / Sharing` = x$Actions,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    output$filtered_table <- DT::renderDT({
      shiny::req(filtered())

      DT::datatable(
        filtered(),
        escape = FALSE, # Required to execute dynamic HTML button scripts
        rownames = FALSE,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-left', targets = "_all"),
            list(width = '240px', targets = 3) # Fixes column size constraints
          )
        ),
        # Automated click intercept binding script for fast copy clipboard routines
        callback = DT::JS(
          "table.on('click', '.dt-copy-btn', function(e) {
             e.preventDefault();
             e.stopPropagation();
             var text = $(this).attr('data-copy-text');
             var $btn = $(this);
             
             if (text && text !== '#') {
               navigator.clipboard.writeText(text).then(function() {
                 var originalText = $btn.text();
                 $btn.text('Copied!').css('background-color', '#00703c').css('color', '#ffffff');
                 setTimeout(function() {
                   $btn.text(originalText).css('background-color', '').css('color', '');
                 }, 1500);
               });
             }
           });"
        )
      )
    })
  })
}
