context("Test table definitions and validation methods")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")


#get all tables
tables <- get_tables()

#use a sample of tables to prevent api from erroring
set.seed(123)
tables_sample <- tables[sample(length(tables), 3)]

test_that("Field are returned", {
  for(i in seq_along(tables_sample))
    expect_true(is.character(get_fields(tables_sample[i], token)))
})
