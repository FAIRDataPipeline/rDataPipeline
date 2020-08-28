context("checking validate_post_data")

sleep_time <- 0.5

key <- Sys.getenv("SCRC_API_TOKEN")

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

test_that("validate_post_date works with all tables",{
  for(i in seq_along(tables)){
    if(tables[i] == "users" | tables[i] == "groups")
      expect_error(validate_post_data(tables[i], data = list("username = test"), token))
    else{
      Sys.sleep(sleep_time)
      data = list()
      table.writable <- get_table_writable(tables[i], key)
      for(field in seq_along(table.writable))
        data[table.writable[field]] <- "test"

      expect_silent(validate_post_data(tables[i], data, key))
      Sys.sleep(sleep_time)
      expect_error(validate_post_data(tables[i], data=list(unknown = "unknown"), key))
      Sys.sleep(sleep_time)#
    }}
})
