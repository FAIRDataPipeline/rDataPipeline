context("Test table definitions and validation methods")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

sleep_time <- 0.1


test_that("Fields are returned by get_fields", {
  for(i in seq_along(tables)){
    if(tables[i] == "users" | tables[i] == "groups")
      expect_error(get_fields(tables[i], token))
    else{
    expect_true(is.character(get_fields(tables[i], token)))
    Sys.sleep(sleep_time)
    expect_true(is.character(get_table_writable(tables[i], token)))
    Sys.sleep(sleep_time)
    expect_true(is.character(get_table_readable(tables[i], token)))
    Sys.sleep(sleep_time)
    expect_true(is.character(get_table_optional(tables[i], token)))
    Sys.sleep(sleep_time)
    expect_true(is.character(get_table_required(tables[i], token)))
    Sys.sleep(sleep_time)
    expect_true(is.character(get_fields(tables[i], token, "read_only")))
    Sys.sleep(sleep_time)
    #default behavior if filter fields is not recognized
    expect_false(is.character(get_fields(tables[i], token, "default")))
    Sys.sleep(sleep_time)
    }
  }
})

test_that("Users and groups behave correctly with is_queryable()",{
  expect_false(check_query("users", list(URL = "1")))
  expect_false(check_query("groups", list(URL = "1")))
})

test_that("Errors occur if table does not exist", {
  expect_error(get_table_writable(unknown_table, token))
  expect_error(get_table_readable(unknown_table, token))
  expect_error(get_table_optional(unknown_table, token))
  expect_error(get_table_required(unknown_table, token))
})
