context("test update_validation")

key <- Sys.getenv("SCRC_API_TOKEN")

test_that("update_validation fails without key",{
  expect_error(SCRCdataAPI:::update_validation())
})

test_that("update_validation creates correct rds files", {
  dir.create("inst/validation", recursive = TRUE)
  SCRCdataAPI:::update_validation(key)
  expect_true(file.exists("inst/validation/tables.rds"))
  tables <- get_tables()
  for(i in seq_along(tables)){
    table = tables[i]
    if(table == "users" | table == "groups"){
      #do nothing
    } else {
      expect_true(file.exists(paste0("inst/validation/", table, ".rds")))
    }
  }
})

unlink("inst", recursive = TRUE)
