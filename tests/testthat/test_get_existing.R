context("Test get_existing()")

#get all tables
tables <- get_tables()
# use a table that has more than 100 entries
table_100 <- "storage_location"

sleep_time <- 0.5

#use a sample of tables to prevent api from erroring
#tables_sample <- tables[sample(length(tables), 3)]

test_that("Expect Dataframe is returned for each table", {
  for(i in seq_along(tables)){
    expect_true(is.data.frame(get_existing(tables[i])))
    # Add a delay to stop the API from erroring
    Sys.sleep(sleep_time)
  }

})

test_that("Check errors and messages", {
  expect_error(get_existing("unknown"))
  Sys.sleep(sleep_time)
  # users and groups will not produce results without key
  expect_message(get_existing("users"))
})

test_that("Check limit works with pagination", {
  Sys.sleep(sleep_time)
  expect_gt(nrow(get_existing(table_100, limit = FALSE)), 100)
})

test_that("detail = id provides correct data.frame", {
  Sys.sleep(sleep_time)
  expect_true(all(utils::hasName(get_existing(table_100, detail = "id"), c("url", "id"))))
})
