test_that("ui_show_news returns a bslib card with news items", {
  fake_df <- data.frame(
    message_date = Sys.time(),
    message_text = "<strong>Important</strong>",
    app_id = 0,
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
  out_char <- as.character(out)

  expect_true(inherits(out, "shiny.tag"))

  expect_match(out_char, "System News.*Alerts")

  expect_match(out_char, "NEW")
  expect_match(out_char, "Important\\.\\.\\.")

  expect_match(out_char, "background-color:#fdecea")
})

test_that("ui_show_news returns fallback HTML on failure", {
  mockery::stub(
    ui_show_news,
    "db_get_portal_messages",
    function() stop("DB down")
  )

  out <- ui_show_news()

  expect_true(inherits(out, "shiny.tag"))
  expect_match(as.character(out), "No news or alerts available")
})
