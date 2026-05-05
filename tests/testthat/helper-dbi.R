# tests/testthat/helper-dbi.R
# DB stubs — target *local* bindings inside functions

stub_db_get_query <- function(fn, result) {
  mockery::stub(
    fn,
    "db_get_query",
    function(conn, query) result
  )
}

stub_db_get_query_error <- function(fn, message = "SQL error") {
  mockery::stub(
    fn,
    "db_get_query",
    function(conn, query) {
      stop(message, call. = FALSE)
    }
  )
}

stub_db_execute <- function(fn, side_effect = NULL) {
  mockery::stub(
    fn,
    "db_execute",
    function(conn, query) {
      if (!is.null(side_effect)) {
        side_effect()
      }
      invisible(1L)
    }
  )
}
