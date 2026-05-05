test_that("sql_manager errors when required fields missing", {
  mockery::stub(
    sql_manager,
    "config::get",
    function(...) {
      list(
        driver = "SQL Server",
        server = "localhost"
        # missing database, uid, pwd, trusted
      )
    }
  )

  expect_error(
    sql_manager("prod"),
    "Missing required DB config fields"
  )
})

test_that("sql_manager calls DBI::dbConnect with expected args", {
  called <- FALSE

  mockery::stub(
    sql_manager,
    "config::get",
    function(...) {
      list(
        driver = "SQL Server",
        server = "myserver",
        database = "mydb",
        uid = "user",
        pwd = "pass",
        trusted = "False"
      )
    }
  )

  mockery::stub(
    sql_manager,
    "DBI::dbConnect",
    function(...) {
      called <<- TRUE
      "fake_connection"
    }
  )

  conn <- sql_manager("prod")

  expect_true(called)
  expect_equal(conn, "fake_connection")
})
