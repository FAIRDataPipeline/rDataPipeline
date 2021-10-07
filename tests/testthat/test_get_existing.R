context("Testing get_existing()")

endpoint <- Sys.getenv("FDP_endpoint")

# Get all tables
tables <- get_tables(endpoint)
tables <- setdiff(tables, c("users", "groups"))

# Use a table that has more than 100 entries
lapply(1:100, function(x) new_object(description = x,
                                     endpoint = endpoint))
table <- "object"

test_that("Expect Dataframe is returned for each table", {
  for (i in seq_along(tables))
    expect_true(is.data.frame(get_existing(table = tables[i],
                                           endpoint = endpoint)))
})

test_that("Check errors and messages", {
  expect_error(get_existing(table = "unknown",
                            endpoint = endpoint))
})

test_that("Check limit works with pagination", {
  expect_gt(nrow(get_existing(table = table,
                              limit = FALSE,
                              endpoint = endpoint)), 100)
})

test_that("detail = id provides correct data.frame", {
  expect_true(all(utils::hasName(get_existing(table = table,
                                              detail = "id",
                                              endpoint = endpoint),
                                 c("url", "id"))))
})
