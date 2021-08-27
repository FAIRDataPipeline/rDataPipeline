context("Testing table_exists()")

tables <- c("users",
            "groups",
            "file_type",
            "issue",
            "author",
            "object",
            "object_component",
            "code_run",
            "storage_root",
            "storage_location",
            "external_object",
            "quality_controlled",
            "keyword",
            "licence",
            "namespace",
            "data_product",
            "code_repo_release",
            "key_value")

endpoint <- Sys.getenv("FDP_endpoint")

test_that("check table returns true with correct tables", {
  tmp <- lapply(seq_along(tables), function(x) {
    check_table_exists(tables[x])
  }) %>% unlist()
  expect_true(all(tmp))
})

test_that("unknown table returns false", {
  expect_false(check_table_exists("unknown"))
})

test_that("invalid table name returns error", {
  expect_error(check_table_exists(NULL))
  expect_error(check_table_exists(NA))
  expect_error(check_table_exists(NaN))
  expect_error(check_table_exists(list()))
  expect_error(check_table_exists(TRUE))
  expect_error(check_table_exists())
})
