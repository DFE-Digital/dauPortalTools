test_that("get_config reads valid config.yml", {
  cfg_file <- tempfile(fileext = ".yml")

  writeLines(
    c(
      "app_details:",
      "  app_id: 7",
      "logging:",
      "  enabled: true",
      "  event_log_path: test.log",
      "  log_to_console: false"
    ),
    cfg_file
  )

  cfg <- get_config(cfg_file)

  expect_equal(cfg$app_details$app_id, 7)
  expect_true(cfg$logging$enabled)
})

test_that("get_config errors when file missing", {
  expect_error(
    get_config("this_file_does_not_exist.yml"),
    "Config file not found"
  )
})

test_that("get_config errors when app_id missing", {
  cfg_file <- tempfile(fileext = ".yml")

  writeLines(
    c(
      "app_details:",
      "  name: test"
    ),
    cfg_file
  )

  expect_error(
    get_config(cfg_file),
    "app_details\\$app_id"
  )
})

test_that("get_config errors when logging section missing", {
  cfg_file <- tempfile(fileext = ".yml")

  writeLines(
    c(
      "app_details:",
      "  app_id: 7"
    ),
    cfg_file
  )

  expect_error(
    get_config(cfg_file),
    "logging section"
  )
})
