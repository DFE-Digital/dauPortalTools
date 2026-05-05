#' Show portal news and alerts table (collapsible, prioritised)
#'
#' Displays a compact table of portal news and alerts with:
#' - Sticky critical messages at the top
#' - "NEW" badge for messages < 48 hours old
#' - Row highlighting by priority
#' - Expand/collapse icon with inline full message
#'
#' @return A DT datatable suitable for direct use in Shiny UI
#' @export
ui_show_news <- function() {
  dauPortalTools::log_event("Starting ui_show_news")

  out <- tryCatch(
    {
      df <- db_get_portal_messages()

      now_utc <- Sys.time()

      df$is_new <- difftime(
        now_utc,
        as.POSIXct(df$message_date, tz = "UTC"),
        units = "hours"
      ) <=
        48

      df$new_badge <- ifelse(
        df$is_new,
        "<span style='background:#005ea5;color:white;
                padding:2px 6px;border-radius:3px;
                font-size:0.75em;margin-right:6px'>NEW</span>",
        ""
      )

      df$preview_text <- vapply(
        df$message_text,
        function(x) {
          txt <- gsub("<[^>]+>", "", x)
          if (nchar(txt) > 180) {
            paste0(substr(txt, 1, 180), "…")
          } else {
            txt
          }
        },
        character(1)
      )

      df$display_text <- paste0(df$new_badge, df$preview_text)

      df$expand_icon <- "\u25B6" # ▶

      df <- df[order(df$priority, df$message_date, decreasing = FALSE), ]

      dt <- DT::datatable(
        df[, c(
          "expand_icon",
          "message_date",
          "display_text",
          "message_text",
          "priority",
          "ad_username"
        )],
        rownames = FALSE,
        escape = FALSE,
        colnames = c(
          "",
          "Date",
          "Message",
          "FullMessage",
          "Priority",
          "Posted by"
        ),
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          searching = FALSE,
          ordering = FALSE,
          dom = "tp",
          columnDefs = list(
            list(targets = c(3, 4), visible = FALSE),
            list(targets = 0, width = "20px")
          ),
          rowCallback = DT::JS(
            "
            function(row, data) {
              var priority = data[4];

              if (priority == 1) {
                $(row).css({'background-color':'#fdecea'});
              } else if (priority == 2) {
                $(row).css({'background-color':'#fff4e5'});
              }
            }
            "
          ),
          callback = DT::JS(
            "
            table.on('click', 'tbody tr', function () {
              var row = table.row(this);
              var iconCell = $('td:eq(0)', this);

              if (row.child.isShown()) {
                row.child.hide();
                iconCell.html('\\u25B6'); // ▶
                $(this).removeClass('shown');
              } else {
                row.child(
                  '<div style=\"padding:10px\">' + row.data()[3] + '</div>'
                ).show();
                iconCell.html('\\u25BC'); // ▼
                $(this).addClass('shown');
              }
            });
            "
          )
        )
      )

      dauPortalTools::log_event(
        glue::glue("Finished ui_show_news ({nrow(df)} messages available)")
      )

      dt
    },
    error = function(e) {
      dauPortalTools::log_event(
        paste0("ui_show_news error: ", conditionMessage(e))
      )

      shiny::HTML(
        "<em>Unable to load news and alerts at this time.</em>"
      )
    }
  )

  out
}
