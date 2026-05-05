# tests/testthat/helper-config.R
# Config override helpers for targeted tests

stub_get_config <- function(fn, new_config) {
  mockery::stub(
    fn,
    "get_config",
    function(...) new_config
  )
}

stub_missing_schemas <- function(fn) {
  mockery::stub(
    fn,
    "get_config",
    function(...) {
      list(
        app_details = list(app_id = 7),
        logging = list(
          enabled = TRUE,
          log_to_console = FALSE,
          event_log_path = tempfile()
        )
      )
    }
  )
}
