test_that("quality_add_log accepts NULL / NA inputs without error", {
  executed <- FALSE

  expect_silent(
    quality_add_log(
      quality_check_id = 42,
      live_issues = NULL,
      new_issues = NA,
      closed_issues = numeric(0),
      db_execute = function(conn, query) {
        executed <<- TRUE
        invisible(1L)
      }
    )
  )

  expect_true(executed)
  expect_match(.last_log_event, "Finished quality_add_log")
})
