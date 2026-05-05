test_that("utils_get_app_id errors when app_id missing", {
  mockery::stub(
    utils_get_app_id,
    "get_config",
    function() {
      list(app_details = list())
    }
  )

  expect_error(
    utils_get_app_id(),
    "app_id is missing"
  )
})
