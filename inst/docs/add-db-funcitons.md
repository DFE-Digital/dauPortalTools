# Adding Database Functions

This guide defines the standard structure, naming conventions, documentation requirements, and testing patterns for adding new database query modules to the package.

---

## 1. File & Function Naming Conventions

| Artefact | Pattern | Example |
| :--- | :--- | :--- |
| **Source File** | `R/db-<domain>.R` | `R/db-dao.R`, `R/db-ofsted.R` |
| **Test File** | `tests/testthat/test-db-<domain>.R` | `tests/testthat/test-db-dao.R` |
| **Read Function** | `db_<domain>_get_<action>()` | `db_dao_get_latest_extract()` |
| **Write Function** | `db_<domain>_<verb>_<target>()` | `db_dao_insert_record()`, `db_dao_update_status()` |

### Rules
1. **Always start with `db_<domain>_`** so autocomplete groups every query for a domain together.
2. **Use `snake_case` for all R function names and returned SQL column aliases**, even if the underlying SQL table uses spaces or special characters (`[School Name] AS [school_name]`).
3. **Every domain file should include a `db_<domain>_get_all()` function** as its first function to expose the base typed schema contract.

---

## 2. What to Edit When Adding a New DB Module

1. **Create `R/db-<domain>.R`** using the canonical template in Section 3.
2. **Check `config.yml` / `utils_resolve_schema()`** to ensure the schema alias (e.g., `"db_schema_00c"`) is registered.
3. **Run `devtools::document()`** to generate the `.Rd` help files and update `NAMESPACE`.
4. **Create `tests/testthat/test-db-<domain>.R`** using the mocking pattern in Section 5.
5. **Run `devtools::test()` and `styler::style_dir()`** before opening a PR.

---

## 3. Canonical File & Function Template

Copy this exact layout when creating a new `R/db-<domain>.R` file.

```r
# ==============================================================================
# File: R/db-<domain>.R
# Purpose: Database query functions for <Domain Name> data
#
# Functions:
#   - db_<domain>_get_all() : Fetch full typed <domain> history from SQL
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all <Domain Name> records
#'
#' Retrieves the complete dataset from the `<TableName>` table, standardising
#' column names into snake_case and casting raw database strings and timestamps
#' into appropriate analytical types (`integer`, `Date`, `logical`, and `numeric`).
#'
#' @details
#' Queries all rows from `[<TableName>]` within the schema resolved by
#' `utils_resolve_schema("<schema_alias>")`.
#'
#' Type conversions are performed in SQL Server using `TRY_CAST()` for numeric
#' identifiers/counts and `CAST(... AS DATE)` for midnight-truncated
#' `datetime` fields.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` (or `tibble`, depending on [utils_db_get_query()])
#'   with N typed columns, or `NULL` if a database connection or query error occurs:
#'   \describe{
#'     \item{`urn`}{Integer. School URN.}
#'     \item{`date_of_extract`}{Date. Snapshot extraction date.}
#'   }
#'
#' @family <domain> database functions
#' @seealso [sql_manager()], [utils_resolve_schema()], [log_event()]
#'
#' @export
#' @examples
#' \dontrun{
#' df <- db_<domain>_get_all()
#' }
db_<domain>_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("<schema_alias>")

  query <- glue::glue(
    "
    SELECT
      TRY_CAST([URN] AS INT) AS [urn],
      [date_of_extract]
    FROM {schema}.[<TableName>];
  "
  )

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}
```

---

## 4. Why We Write It This Way

Every line in the template exists for a specific operational reason:

* **`fn_name <- as.character(match.call()[[1]])`**  
  Captures the function name dynamically at runtime. If you copy-paste a function to create a new one, you cannot accidentally leave the old function's name in the logs.
* **`log_event(paste("Starting", fn_name), debug = TRUE)`**  
  Start messages are noisy during normal production runs, so `debug = TRUE` hides them unless `debug_toggle` is enabled in `config.yml`. Meanwhile, the `"Finished ..."` message logs at standard `Info` level so production logs still record successful completions and execution duration.
* **`on.exit(..., add = TRUE)`**  
  Guarantees cleanup happens even if an error is thrown mid-function. Always pass `add = TRUE`—without it, a second `on.exit()` call overwrites the first one, which would prevent `log_event("Finished ...")` from running when `dbDisconnect()` is registered.
* **`conn <- sql_manager(conn_name)` inside `tryCatch()`**  
  If the VPN drops, credentials expire, or SQL Server times out while opening a connection, `sql_manager()` throws an error. Placing it inside `tryCatch()` ensures connection failures are caught and logged at `type = 2` (`Error`) rather than crashing the Shiny session.
* **Returning `NULL` on error (instead of `0L` or `FALSE`)**  
  Downstream callers expect a `data.frame` (or `NULL` when unavailable). Returning an atomic scalar like `0L` causes cryptic downstream errors (`$ operator is invalid for atomic vectors`), whereas `is.null(df)` is clean and predictable.
* **Resolving `schema` once in R before `glue::glue()`**  
  Avoids calling `utils_resolve_schema()` multiple times inside subqueries/CTEs and keeps the SQL string readable.
* **Casting types in SQL (`TRY_CAST`, `CAST AS DATE`, `BIT`)**  
  Legacy SQL tables often store numbers and flags as `varchar(255)` or midnight `datetime`s. Casting in SQL enforces a strict data contract before data enters R, and `TRY_CAST()` safely turns malformed legacy strings into `NA` rather than failing the query.
* **Using `utils_db_get_query()` / `utils_db_execute()` instead of raw `DBI::`**  
  Because `DBI::dbGetQuery()` is an S4 generic in an external namespace, mocking it cleanly in `testthat` is awkward. Our thin internal wrappers live in our own package namespace, making `testthat::local_mocked_bindings()` trivial.

---

## 5. Roxygen2 Documentation Checklist

Every exported `db_*` function must include:

1. **Title & Description:** Concise summary of what is fetched and how it is transformed.
2. **`@details`:** The underlying SQL table/schema alias and any non-obvious filtering or SQL type-casting rules.
3. **`@section Side Effects:`** Document DB connections opened/closed and `log_event()` writes.
4. **`@param conn_name`:** Standard description referencing `[sql_manager()]` and default `"dit"`.
5. **`@return`:** Explicitly state the return type (`data.frame` or `NULL` on error) and document **every output column** with its R type (`Integer`, `Character`, `Date`, `Logical`, `Numeric`) using `\describe{}`. If a column has a fixed set of categorical values (e.g., status codes), list them.
6. **`@family` & `@seealso`:** Group all functions in the file under `@family <domain> database functions` so `pkgdown` cross-links them automatically.
7. **`@examples`:** Always wrap live DB calls in `\dontrun{}` so `R CMD check` passes in offline CI environments.

---

## 6. How to Write Unit Tests

Never connect to a live database in unit tests. Instead, use `testthat::local_mocked_bindings()` to stub out the internal helper functions:

1. **Mock `sql_manager()`** to return a dummy string/object (e.g., `"mock_conn"`). Note that `try(DBI::dbDisconnect(conn), silent = TRUE)` in `on.exit()` safely ignores the dummy object.
2. **Mock `utils_resolve_schema()`** to return a predictable test schema (e.g., `"[00_Core]"`).
3. **Mock `log_event()`** to silence console/disk output during test runs (or capture calls to verify error logging).
4. **Mock `utils_db_get_query()`** to inspect the generated SQL string and return a controlled fixture `data.frame`, or call `stop("DB timeout")` to test the `tryCatch()` failure path.

### Minimum Test Coverage per Function
* **Happy path:** Verifies the returned `data.frame` passes through cleanly and passes the expected `conn_name` to `sql_manager()`.
* **SQL generation:** Verifies the rendered SQL query targets the resolved schema, table, and expected column aliases/filters.
* **Error handling:** Verifies that when `sql_manager()` or `utils_db_get_query()` throws an error, the function catches it, logs with `type = 2`, and returns `NULL`.