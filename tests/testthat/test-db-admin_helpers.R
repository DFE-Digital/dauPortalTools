# tests/testthat/test-db-portal_user_admin_helpers.R

test_that("db_get_app_users returns user-role data", {
  fake_df <- data.frame(
    user_id = c(1, 2),
    username = c("alice", "bob"),
    email = c("a@test", "b@test"),
    role_name = c("Admin", "User"),
    role_id = c(1, NA),
    stringsAsFactors = FALSE
  )

  testthat::with_mocked_bindings(
    utils_db_get_query = function(conn, query) fake_df,
    utils_get_app_id = function() 7,
    utils_resolve_schema = function(...) "dbo",
    # FIX: Just call the function to get the real S4 object
    sql_manager = function(...) DBI::ANSI(),
    {
      out <- db_get_app_users()

      expect_equal(nrow(out), 2)
      expect_equal(out$username[1], "alice")
      expect_equal(out$role_name[2], "User")
    }
  )
})


test_that("db_get_roles returns roles", {
  fake_df <- data.frame(
    role_id = c(1, 2),
    role_name = c("Admin", "User"),
    stringsAsFactors = FALSE
  )

  testthat::with_mocked_bindings(
    utils_db_get_query = function(conn, query) fake_df,
    utils_resolve_schema = function(...) "dbo",
    # FIX: Just call the function
    sql_manager = function(...) DBI::ANSI(),
    {
      out <- db_get_roles()

      expect_equal(nrow(out), 2)
      expect_equal(out$role_name[2], "User")
    }
  )
})


test_that("db_update_user_role executes update", {
  executed <- FALSE

  testthat::with_mocked_bindings(
    utils_db_execute = function(conn, query) {
      executed <<- TRUE
      invisible(1L)
    },
    utils_resolve_schema = function(...) "dbo",
    # FIX: Just call the function
    sql_manager = function(...) DBI::ANSI(),
    {
      db_update_user_role(
        user_id = 1,
        role_id = 2,
        app_id = 7,
        assigned_by = 3
      )

      expect_true(executed)
    }
  )
})
