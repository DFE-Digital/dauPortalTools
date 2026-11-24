# ==============================================================================
# File: tests/testthat/test-db-dao.R
# Purpose: Unit tests for R/db-dao.R using mocked DB and logging helpers
# ==============================================================================

test_that("db_dao_get_all queries the resolved schema and returns query results", {
  captured <- new.env(parent = emptyenv())

  mock_df <- data.frame(
    urn = 107560L,
    school_name = "St Joseph's RC Primary School, Todmorden",
    sponsor_match = TRUE,
    over_9_months_flag = TRUE,
    date_of_extract = as.Date("2026-01-01"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    log_event = function(...) invisible(NULL),
    utils_resolve_schema = function(alias) {
      captured$schema_alias <- alias
      "[00_Core]"
    },
    sql_manager = function(service, ...) {
      captured$service <- service
      "mock_conn"
    },
    utils_db_get_query = function(conn, query) {
      captured$conn <- conn
      captured$query <- as.character(query)
      mock_df
    }
  )

  res <- db_dao_get_all(conn_name = "dit_test")

  expect_equal(res, mock_df)
  expect_equal(captured$schema_alias, "db_schema_00c")
  expect_equal(captured$service, "dit_test")
  expect_equal(captured$conn, "mock_conn")
  expect_match(captured$query, "FROM \\[00_Core\\]\\.\\[dAOList\\]")
  expect_match(captured$query, "TRY_CAST\\(\\[URN\\] AS INT\\) AS \\[urn\\]")
  expect_no_match(captured$query, "WHERE \\[date_of_extract\\]")
})

test_that("db_dao_get_latest_extract filters to MAX(date_of_extract)", {
  captured <- new.env(parent = emptyenv())

  mock_df <- data.frame(
    urn = 120713L,
    dao_status = "Closed to become an Academy",
    date_of_extract = as.Date("2026-01-01"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    log_event = function(...) invisible(NULL),
    utils_resolve_schema = function(alias) "[00_Core]",
    sql_manager = function(service, ...) "mock_conn",
    utils_db_get_query = function(conn, query) {
      captured$query <- as.character(query)
      mock_df
    }
  )

  res <- db_dao_get_latest_extract()

  expect_equal(res, mock_df)
  expect_match(captured$query, "WHERE \\[date_of_extract\\] = \\(")
  expect_match(captured$query, "SELECT MAX\\(\\[date_of_extract\\]\\)")
})

test_that("db_dao_get_historical_timeline queries TOP (12) extracts ordered ascending", {
  captured <- new.env(parent = emptyenv())

  mock_df <- data.frame(
    date_of_extract = as.Date(c("2025-12-01", "2026-01-01")),
    dao_status = c("Awaiting conversion", "Awaiting conversion"),
    over_9_months_flag = c(FALSE, TRUE),
    school_count = c(42L, 45L),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    log_event = function(...) invisible(NULL),
    utils_resolve_schema = function(alias) "[00_Core]",
    sql_manager = function(service, ...) "mock_conn",
    utils_db_get_query = function(conn, query) {
      captured$query <- as.character(query)
      mock_df
    }
  )

  res <- db_dao_get_historical_timeline()

  expect_equal(res, mock_df)
  expect_match(
    captured$query,
    "SELECT DISTINCT TOP \\(12\\) \\[date_of_extract\\]"
  )
  expect_match(
    captured$query,
    "COUNT\\(DISTINCT d\\.\\[URN\\]\\) AS \\[school_count\\]"
  )
  expect_match(captured$query, "ORDER BY d\\.\\[date_of_extract\\] ASC")
})

test_that("db_dao_* functions catch query errors, log at type = 2, and return NULL", {
  logs <- list()

  testthat::local_mocked_bindings(
    log_event = function(message, type = 0, debug = FALSE) {
      logs[[length(logs) + 1]] <<- list(
        message = message,
        type = type,
        debug = debug
      )
      invisible(NULL)
    },
    utils_resolve_schema = function(alias) "[00_Core]",
    sql_manager = function(service, ...) "mock_conn",
    utils_db_get_query = function(conn, query) {
      stop("Simulated ODBC timeout")
    }
  )

  expect_null(db_dao_get_all())
  expect_null(db_dao_get_latest_extract())
  expect_null(db_dao_get_historical_timeline())

  error_logs <- Filter(function(x) identical(x$type, 2), logs)
  expect_length(error_logs, 3)
  expect_match(
    error_logs[[1]]$message,
    "db_dao_get_all failed: Simulated ODBC timeout"
  )
  expect_match(
    error_logs[[2]]$message,
    "db_dao_get_latest_extract failed: Simulated ODBC timeout"
  )
  expect_match(
    error_logs[[3]]$message,
    "db_dao_get_historical_timeline failed: Simulated ODBC timeout"
  )
})

test_that("db_dao_* functions catch connection failures in sql_manager()", {
  logs <- list()

  testthat::local_mocked_bindings(
    log_event = function(message, type = 0, debug = FALSE) {
      logs[[length(logs) + 1]] <<- list(
        message = message,
        type = type,
        debug = debug
      )
      invisible(NULL)
    },
    utils_resolve_schema = function(alias) "[00_Core]",
    sql_manager = function(service, ...) {
      stop("Missing required DB config fields: pwd")
    }
  )

  expect_null(db_dao_get_latest_extract())

  error_logs <- Filter(function(x) identical(x$type, 2), logs)
  expect_length(error_logs, 1)
  expect_match(
    error_logs[[1]]$message,
    "db_dao_get_latest_extract failed: Missing required DB config fields: pwd"
  )
})
