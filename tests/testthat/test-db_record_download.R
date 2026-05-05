test_that("db_record_download executes analytics insert", {
  executed <- FALSE

  db_record_download(
    user = "ben.smith",
    page_name = "School Details",
    file_name = "school_data.csv",
    db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    }
  )

  expect_true(executed)
  expect_match(.last_log_event, "Finished record_download")
})
