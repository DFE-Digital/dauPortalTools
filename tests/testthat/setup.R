# Capture logging
assignInNamespace(
  "log_event",
  function(msg) {
    assign(".last_log_event", msg, envir = .GlobalEnv)
    invisible(NULL)
  },
  ns = "dauPortalTools"
)

# Config stub (schema keys only)
assignInNamespace(
  "get_config",
  function(...) {
    list(
      app_details = list(app_id = 7),
      schemas = list(
        "00c" = "[TEST_00c]",
        "01a" = "[TEST_01a]",
        "01s" = "[TEST_01s]",
        "02s" = "[TEST_02s]"
      ),
      logging = list(
        enabled = TRUE,
        log_to_console = FALSE,
        event_log_path = tempfile(),
        summary_log_dir = tempfile()
      )
    )
  },
  ns = "dauPortalTools"
)

# Fake connection — must NOT be DBI-like
assignInNamespace(
  "sql_manager",
  function(...) {
    list(.fake = TRUE)
  },
  ns = "dauPortalTools"
)
