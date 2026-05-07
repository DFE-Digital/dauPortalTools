#' @export
ui_news_dependencies <- function() {
  # This returns the specific HTML dependencies for DataTables
  # without rendering an actual table.
  htmltools::tagList(
    DT::datatable(matrix(0, 0, 0)) |>
      htmlwidgets::getDependency("datatables", "DT")
  )
}
