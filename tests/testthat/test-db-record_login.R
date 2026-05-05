test_that("db_record_login executes analytics insert", {
  executed <- FALSE

  db_record_login(
    user = "ben.smith",
    db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    }
  )

  expect_true(executed)
  expect_match(.last_log_event, "Finished db_record_login")
})
