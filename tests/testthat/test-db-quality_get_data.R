test_that("quality_get_data returns all unresolved issues when no record filter", {
  fake_df <- data.frame(
    `Quality Concern` = c("Missing data", "Invalid value"),
    Description = c("Desc 1", "Desc 2"),
    `Date Identified` = as.Date(c("2024-01-01", "2024-01-02")),
    `Last Reviewed` = as.Date(c("2024-01-10", "2024-01-11")),
    stringsAsFactors = FALSE
  )

  out <- quality_get_data(
    db_get_query = function(conn, query) fake_df
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out[[1]][1], "Missing data")
  expect_match(.last_log_event, "Finished quality_get_data")
})


test_that("quality_get_data applies record filter when provided", {
  fake_df <- data.frame(
    `Quality Concern` = "Missing data",
    Description = "Desc 1",
    `Date Identified` = as.Date("2024-01-01"),
    `Last Reviewed` = as.Date("2024-01-10"),
    stringsAsFactors = FALSE
  )

  out <- quality_get_data(
    record = 12345,
    db_get_query = function(conn, query) fake_df
  )

  expect_equal(nrow(out), 1)
  expect_equal(out[[1]][1], "Missing data")
  expect_match(.last_log_event, "Finished quality_get_data")
})
