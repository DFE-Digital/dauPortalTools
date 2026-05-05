test_that("ui_show_news returns a DT widget", {
  fake_df <- data.frame(
    message_date = Sys.time(),
    message_text = "<strong>Important</strong>",
    app_id = 7,
    priority = 1,
    ad_username = "ben",
    stringsAsFactors = FALSE
  )

  mockery::stub(
    ui_show_news,
    "db_get_portal_messages",
    function() fake_df
  )

  out <- ui_show_news()

  expect_true(inherits(out, "htmlwidget"))
  expect_match(.last_log_event, "Finished ui_show_news")
})

test_that("ui_show_news returns fallback HTML on failure", {
  mockery::stub(
    ui_show_news,
    "db_get_portal_messages",
    function() rlang::abort("DB down")
  )

  out <- ui_show_news()

  expect_true(inherits(out, "html"))
  expect_match(as.character(out), "Unable to load news")
  expect_match(.last_log_event, "ui_show_news error")
})
