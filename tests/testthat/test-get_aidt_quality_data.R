# # test-get_aidt_quality_data.R
#
# test_that("get_aidt_quality_data returns a data frame with expected columns", {
#   # Mock config
#   config <- list(
#     app_details = list(app_id = 1),
#     logging = list(enabled = FALSE, log_to_console = FALSE)
#   )
#
#   # Mock log_event and log_summary to avoid side effects
#   assign("log_event", function(message, config) invisible(NULL), envir = .GlobalEnv)
#   assign("log_summary", function(summary, config) invisible(NULL), envir = .GlobalEnv)
#
#   # Mock DBI::dbGetQuery
#   mock_data <- data.frame(
#     quality_name = "Missing URN",
#     quality_description = "URN is missing from record",
#     quality_status = 0,
#     date_created = as.Date("2023-01-01"),
#     last_checked = as.Date("2023-06-01")
#   )
#
#   assign("sql_manager", function(x) NULL, envir = .GlobalEnv)
#   assign("DBI::dbGetQuery", function(conn, query) mock_data, envir = .GlobalEnv)
#
#   # Run the function
#   result <- get_aidt_quality_data(record = NULL)
#
#   # Check result is a data frame
#   expect_s3_class(result, "data.frame")
#
#   # Check expected columns
#   expect_true(all(c(
#     "Quality Concern",
#     "Description",
#     "Resolution Status",
#     "Date Identified",
#     "Last Reviewed"
#   ) %in% colnames(result)))
#
#   # Check data content
#   expect_equal(result$`Quality Concern`, "Missing URN")
#   expect_equal(result$`Resolution Status`, 0)
# })
