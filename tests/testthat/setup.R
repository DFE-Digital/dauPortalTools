# Capture logging
assignInNamespace(
  "log_event",
  function(msg) {
    assign(".last_log_event", msg, envir = .GlobalEnv)
    invisible(NULL)
  },
  ns = "dauPortalTools"
)

# Config stub — MUST match canonical schema keys
assignInNamespace(
  "get_config",
  function(...) {
    list(
      app_details = list(app_id = 7),
      schemas = list(
        db_schema_00c = "[TEST_00c]",
        db_schema_01a = "[TEST_01a]",
        db_schema_01sc = "[TEST_01sc]",
        db_schema_02s = "[TEST_02s]"
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
