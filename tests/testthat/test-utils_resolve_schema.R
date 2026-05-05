test_that("utils_resolve_schema resolves a valid schema key and logs success", {
  out <- utils_resolve_schema("01a")

  expect_true(methods::is(out, "SQL"))
  expect_equal(as.character(out), "[TEST_01a]")

  expect_match(
    .last_log_event,
    "resolved schema key 'db_schema_01a'"
  )
})


test_that("utils_resolve_schema errors on unknown schema key and logs failure", {
  expect_error(
    utils_resolve_schema("unknown"),
    "Unknown schema key"
  )

  expect_match(
    .last_log_event,
    "unknown schema key 'unknown'"
  )
})


test_that("utils_resolve_schema errors on invalid input and logs failure", {
  expect_error(
    utils_resolve_schema(NA),
    "Schema key must be"
  )

  expect_match(
    .last_log_event,
    "invalid schema key input"
  )
})
