context("Test table definitions and validation methods")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

#use a sample of tables to prevent api from erroring
set.seed(123)
tables_sample <- tables[sample(length(tables), 3)]

test_that("Fields are returned by get_fields", {
  for(i in seq_along(tables_sample)){
    expect_true(is.character(get_fields(tables_sample[i], token)))
    expect_true(is.character(get_table_writable(tables_sample[i], token)))
    expect_true(is.character(get_table_readable(tables_sample[i], token)))
    expect_true(is.character(get_table_optional(tables_sample[i], token)))
    expect_true(is.character(get_table_required(tables_sample[i], token)))
    expect_true(is.character(get_fields(tables_sample[i], token, "read_only")))
    #default behavior if filter fields is not recognized
    expect_false(is.character(get_fields(tables_sample[i], token, "default")))
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
