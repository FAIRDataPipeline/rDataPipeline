context("Testing table_definitions() and validation methods")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

sleep_time <- 0.5

column_names <- c("field", "data_type", "read_only", "required", "min_value", "max_value")


test_that("Fields are returned by get_fields", {
  for(i in seq_along(tables)){
    if(tables[i] == "users" | tables[i] == "groups")
      expect_error(get_fields(tables[i], token))
    else{
    expect_true(is.data.frame(get_fields(tables[i], token)))
    expect_true(all(column_names %in% names(get_fields(tables[i], token))))
    expect_true(is.data.frame(get_table_writable(tables[i], token)))
    expect_true(all(column_names %in% names(get_table_writable(tables[i], token))))
    expect_true(all(column_names %in% names(get_table_readable(tables[i], token))))
    expect_true(is.data.frame(get_table_optional(tables[i], token)))
    expect_true(all(column_names %in% names(get_table_optional(tables[i], token))))
    expect_true(is.data.frame(get_table_required(tables[i], token)))
    expect_true(all(column_names %in% names(get_table_required(tables[i], token))))
    }
  }
})

test_that("Fields are returned by get_fields", {
  for(i in seq_along(tables)){
    if(tables[i] == "users" | tables[i] == "groups")
      expect_error(get_fields(tables[i], token, TRUE))
    else{
      expect_true(is.data.frame(get_fields(tables[i], token, TRUE)))
      Sys.sleep(sleep_time)
      expect_true(all(column_names %in% names(get_fields(tables[i], token, TRUE))))
      Sys.sleep(sleep_time)
    }
  }
})


test_that("Users and groups behave correctly with is_queryable()",{
  expect_false(check_query("users", list(URL = "1")))
  Sys.sleep(sleep_time)
  expect_false(check_query("groups", list(URL = "1")))
  Sys.sleep(sleep_time)
})

test_that("Errors occur if table does not exist", {
  expect_error(get_table_writable(unknown_table, token))
  Sys.sleep(sleep_time)
  expect_error(get_table_readable(unknown_table, token))
  Sys.sleep(sleep_time)
  expect_error(get_table_optional(unknown_table, token))
  Sys.sleep(sleep_time)
  expect_error(get_table_required(unknown_table, token))
})
