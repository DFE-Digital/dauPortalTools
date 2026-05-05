test_that("db_get_sig_change_w_region returns query results", {
  fake_df <- data.frame(
    sig_change_id = 1,
    URN = 123456,
    gor_name = "South East",
    DateStamp = as.Date("2024-01-01"),
    stringsAsFactors = FALSE
  )

  out <- db_get_sig_change_w_region(
    db_get_query = function(conn, query) fake_df
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(out$gor_name[1], "South East")
  expect_match(.last_log_event, "Finished db_get_sig_change_w_region")
})
