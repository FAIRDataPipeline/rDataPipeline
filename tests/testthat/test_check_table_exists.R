context("test table_exists")

sleep_time <- 0.5

tables <- c( "users",
             "groups",
             "issue",
             "object",
             "object_component",
             "code_run",
             "storage_root",
             "storage_location",
             "source",
             "external_object",
             "quality_controlled",
             "keyword",
             "author",
             "licence",
             "namespace",
             "data_product",
             "code_repo_release",
             "key_value",
             "text_file")

test_that("check table returns true with correct tables", {
  for(i in seq_along(tables)){
    expect_true(check_table_exists(tables[i]))
    Sys.sleep(sleep_time)
  }
})

test_that("unknown table returns false", {
  expect_false(check_table_exists("unknown"))
})

test_that("invalid table name, produces and error", {
  expect_error(check_table_exists(NULL))
  expect_error(check_table_exists(NA))
  expect_error(check_table_exists(NaN))
  expect_error(check_table_exists(list()))
  expect_error(check_table_exists(TRUE))
})
