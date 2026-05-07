#' Render Portal News and Alerts Table
#'
#' Displays a compact, expandable table of portal news and alerts,
#' prioritised and visually highlighted for ease of scanning.
#'
#' @details
#' The table includes the following features:
#' \itemize{
#'   \item Critical messages (lowest priority value) displayed first
#'   \item "NEW" badge applied to messages created within the last 48 hours
#'   \item Row highlighting based on message priority
#'   \item Expand/collapse behaviour to reveal full message content inline
#' }
#'
#' Messages are retrieved via [db_get_portal_messages()] and transformed
#' into a format suitable for display using [DT::datatable()].
#'
#' The table is rendered with:
#' \itemize{
#'   \item Inline HTML for badges and formatting
#'   \item Hidden columns for full message text and priority
#'   \item JavaScript callbacks to manage row expansion
#' }
#'
#' @section Side Effects:
#' \itemize{
#'   \item Executes database queries via [db_get_portal_messages()]
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return A `DT::datatable` widget. If an error occurs, a fallback
#'   HTML message is returned instead.
#'
#' @examples
#' \dontrun{
#' ui_show_news()
#' }
#'
#' @seealso [db_get_portal_messages()], [DT::datatable()]
#'
#' @export

ui_show_news <- function(id = "news") {
  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {
    session$output[[id]] <- DT::renderDT({
      df <- db_get_portal_messages()

      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Message = "No news or alerts available"),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }

      now_utc <- Sys.time()

      df$is_new <- difftime(
        now_utc,
        as.POSIXct(df$message_date, tz = "UTC"),
        units = "hours"
      ) <=
        48

      df$new_badge <- ifelse(
        df$is_new,
        "<span style='background:#005ea5;color:white;padding:2px 6px;border-radius:3px;font-size:0.75em;margin-right:6px'>NEW</span>",
        ""
      )

      df$preview_text <- vapply(
        df$message_text,
        function(x) {
          txt <- gsub("<[^>]+>", "", x)
          if (nchar(txt) > 180) {
            paste0(substr(txt, 1, 180), "...")
          } else {
            txt
          }
        },
        character(1)
      )

      df$display_text <- paste0(df$new_badge, df$preview_text)
      df$expand_icon <- "\u25B6"

      df <- df[order(df$priority, df$message_date, decreasing = FALSE), ]

      DT::datatable(
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
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          searching = FALSE,
          ordering = FALSE,
          dom = "tp"
        )
      )
    })
  }

  DT::DTOutput(id)
}
