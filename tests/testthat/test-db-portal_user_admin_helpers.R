test_that("db_get_app_users returns user-role data", {
  fake_df <- data.frame(
    user_id = c(1, 2),
    username = c("alice", "bob"),
    email = c("a@test", "b@test"),
    role_name = c("Admin", "User"),
    role_id = c(1, NA),
    stringsAsFactors = FALSE
  )

  out <- db_get_app_users(
    app_id = 7,
    db_get_query = function(conn, query) fake_df
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$username[1], "alice")
})


test_that("db_get_roles returns roles", {
  fake_df <- data.frame(
    role_id = c(1, 2),
    role_name = c("Admin", "User")
  )

  out <- db_get_roles(
    db_get_query = function(conn, query) fake_df
  )

  expect_equal(out$role_name[2], "User")
})


test_that("db_update_user_role executes update", {
  executed <- FALSE

  db_update_user_role(
    user_id = 1,
    role_id = 2,
    app_id = 7,
    assigned_by = 3,
    db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    }
  )

  expect_true(executed)
})
