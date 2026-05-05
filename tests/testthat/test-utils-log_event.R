test_that("log_event writes nothing when logging disabled", {
  with_real_function("log_event", {
    mockery::stub(
      log_event,
      "get_config",
      function() {
        list(
          logging = list(
            enabled = FALSE,
            event_log_path = tempfile(),
            log_to_console = FALSE
          )
        )
      }
    )

    expect_invisible(log_event("Should not log"))
  })
})

test_that("log_event creates directory and writes log entry", {
  with_real_function("log_event", {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    tmp_file <- file.path(tmp_dir, "events.log")

    mockery::stub(
      log_event,
      "get_config",
      function() {
        list(
          logging = list(
            enabled = TRUE,
            event_log_path = tmp_file,
            log_to_console = FALSE
          )
        )
      }
    )

    log_event("Test message")

    expect_true(file.exists(tmp_file))
    expect_match(readLines(tmp_file), "Test message")
  })
})

test_that("log_event prints to console when enabled", {
  with_real_function("log_event", {
    mockery::stub(
      log_event,
      "get_config",
      function() {
        list(
          logging = list(
            enabled = TRUE,
            event_log_path = tempfile(),
            log_to_console = TRUE
          )
        )
      }
    )

    expect_message(
      log_event("Console message"),
      "Console message"
    )
  })
})
