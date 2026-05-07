#' Create a Shiny Download Button with Handler
#'
#' Generates a GOV.UK-styled download button and registers an associated
#' `downloadHandler()` within the current Shiny session. The handler supports
#' both static and reactive data sources and records download activity.
#'
#' @param df A `data.frame`, or a reactive expression/function returning a
#'   `data.frame`.
#' @param file_label1 Character scalar. Primary label used to construct the
#'   output filename.
#' @param file_label2 Character scalar or `NULL`. Optional secondary label
#'   appended to the filename.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Generates a unique output ID for the download control
#'   \item Builds a safe, normalised filename from the supplied labels
#'   \item Supports both reactive and static datasets
#'   \item Registers a `downloadHandler()` within the active Shiny session
#'   \item Infers the current page name from Shiny inputs where possible
#'   \item Records download activity using [record_download()]
#' }
#'
#' Filenames are automatically sanitised to contain only lowercase
#' alphanumeric characters and underscores.
#'
#' A progress indicator is shown while the file is being generated.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Registers a download handler in the active Shiny session
#'   \item Writes a CSV file to disk during download
#'   \item Logs download events via [record_download()]
#' }
#'
#' @return A `shiny::downloadButton` UI element.
#'
#' @examples
#' \dontrun{
#' download_handler(
#'   df = my_data,
#'   file_label1 = "quality_data"
#' )
#'
#' download_handler(
#'   df = reactive(filtered_data()),
#'   file_label1 = "records",
#'   file_label2 = "north_west"
#' )
#' }
#'
#' @seealso [shiny::downloadHandler()], [record_download()]
#'
#' @export

download_handler <- function(
  df,
  file_label1,
  file_label2 = NULL
) {
  safe_label <- function(x) {
    if (is.null(x)) {
      return("")
    }
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- tolower(x)
    trimws(x, which = "both", whitespace = "_")
  }

  infer_page_name <- function(session) {
    if (!is.null(session$input$`router$page`)) {
      return(session$input$`router$page`)
    }

    if (!is.null(session$input$tabs)) {
      return(session$input$tabs)
    }

    if (!is.null(session$input$tab)) {
      return(session$input$tab)
    }

    if (!is.null(session$userData$page_name)) {
      return(session$userData$page_name)
    }

    return("Unknown Page")
  }

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  hash_id <- digest::digest(paste(file_label1, file_label2, rand_id))
  dl_id <- paste0("auto_dl_", hash_id)

  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {
    session$onFlushed(
      function() {
        session$output[[dl_id]] <- shiny::downloadHandler(
          filename = function() {
            paste0(
              safe_label(file_label1),
              if (!is.null(file_label2)) paste0("_", safe_label(file_label2)),
              "_",
              format(Sys.Date(), "%Y%m%d"),
              ".csv"
            )
          },

          content = function(file) {
            data_to_write <- shiny::isolate(
              if (shiny::is.reactive(df) || is.function(df)) df() else df
            )

            filename_final <- paste0(
              safe_label(file_label1),
              if (!is.null(file_label2)) paste0("_", safe_label(file_label2)),
              "_",
              format(Sys.Date(), "%Y%m%d"),
              ".csv"
            )

            page_name_auto <- try(infer_page_name(session), silent = TRUE)
            if (
              inherits(page_name_auto, "try-error") || is.null(page_name_auto)
            ) {
              page_name_auto <- "Unknown Page"
            }

            shiny::withProgress(
              message = "Preparing your download...",
              value = 0,
              {
                utils::write.csv(
                  data_to_write,
                  file,
                  row.names = FALSE,
                  na = ""
                )
                incProgress(1)
              }
            )

            record_download(
              user = session$user %||% "Guest",
              page_name = page_name_auto,
              file_name = filename_final
            )
          },

          contentType = "text/csv"
        )
      },
      once = TRUE
    )
  }

  shiny::downloadButton(
    outputId = dl_id,
    label = "Download CSV",
    class = "govuk-button govuk-button--secondary"
  )
}
