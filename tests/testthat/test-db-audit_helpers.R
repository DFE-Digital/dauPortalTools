test_that("db_record_login executes analytics insert", {
  executed <- FALSE

  testthat::with_mocked_bindings(
    utils_db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    },
    sql_manager = function(...) DBI::ANSI(),
    {
      db_record_login(user = "ben.smith")
    }
  )

  expect_true(executed)
  expect_match(.last_log_event, "Finished db_record_login")
})


test_that("db_record_login executes analytics insert", {
  executed <- FALSE

  testthat::with_mocked_bindings(
    utils_db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    },
    sql_manager = function(...) DBI::ANSI(),
    {
      db_record_login(user = "ben.smith")
    }
  )

  expect_true(executed)
  expect_match(.last_log_event, "Finished db_record_login")
})
