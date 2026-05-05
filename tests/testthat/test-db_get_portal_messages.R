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
