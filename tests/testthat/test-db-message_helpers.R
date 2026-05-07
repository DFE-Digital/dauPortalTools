test_that("db_add_portal_message inserts app-scoped message", {
  executed <- FALSE

  # Stub SQL construction
  mockery::stub(
    db_add_portal_message,
    "glue_sql",
    function(...) DBI::SQL("-- stub sql")
  )

  # Stub DB execution
  mockery::stub(
    db_add_portal_message,
    "dbExecute",
    function(...) {
      executed <<- TRUE
      invisible(1)
    }
  )

  res <- db_add_portal_message(
    message_text = "<strong>Hello</strong>",
    priority = 2,
    ad_username = "alice"
  )

  expect_true(res)
  expect_true(executed)
  expect_match(.last_log_event, "Finished db_add_portal_message")
})


test_that("db_add_portal_message inserts global message when forced", {
  executed <- FALSE

  mockery::stub(
    db_add_portal_message,
    "glue_sql",
    function(...) DBI::SQL("-- stub sql")
  )

  mockery::stub(
    db_add_portal_message,
    "dbExecute",
    function(...) {
      executed <<- TRUE
      invisible(1)
    }
  )

  res <- db_add_portal_message(
    message_text = "Global message",
    priority = 1,
    ad_username = "ben",
    force_catch_all = TRUE
  )

  expect_true(res)
  expect_true(executed)
  expect_match(.last_log_event, "Finished db_add_portal_message")
})

test_that("db_get_portal_messages returns a data frame", {
  fake_df <- data.frame(
    message_date = Sys.time(),
    message_text = "<strong>Hello</strong>",
    app_id = 7,
    priority = 2,
    ad_username = "alice",
    stringsAsFactors = FALSE
  )

  mockery::stub(
    db_get_portal_messages,
    "glue_sql",
    function(...) DBI::SQL("-- stub sql")
  )

  mockery::stub(
    db_get_portal_messages,
    "dbGetQuery",
    function(...) fake_df
  )

  out <- db_get_portal_messages()

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_match(.last_log_event, "Finished db_get_portal_messages")
})


test_that("db_get_portal_messages errors cleanly on SQL failure", {
  mockery::stub(
    db_get_portal_messages,
    "glue_sql",
    function(...) DBI::SQL("-- stub sql")
  )

  mockery::stub(
    db_get_portal_messages,
    "dbGetQuery",
    function(...) stop("SQL error")
  )

  expect_error(
    db_get_portal_messages(),
    "SQL error"
  )
})
