context("checking validate_post_data")

sleep_time <- 0.5

key <- Sys.getenv("SCRC_API_TOKEN")

test_user <- "22"

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

if(is.null(object_id)){
  source_id <- post_data("object",
                         list(name = UID, abbreviation = formatted_date),
                         key)
}

test_that("incorrect tables produce and error", {
  expect_error(validate_post_data(unknown_table, data = list(), key))
  expect_error(validate_post_data(NULL, data = list(), key))
})

test_that("validate_post_data works with all tables",{
  for(i in seq_along(tables)){
    table <- tables[i]
    if(table == "users" | table == "groups")
      expect_error(validate_post_data(tables[i], data = list("username = test"), key))
    else{
      Sys.sleep(sleep_time)
      table.writable <- get_table_writable(table, key)
      Sys.sleep(sleep_time)
      table.required <- get_table_required(table, key)
      if(nrow(table.required > 1)){
        expect_error(validate_post_data(table, data = list(), key))
      }
      data_correct <- list()
      data_correct_2 <- list()
      data_correct_3 <- list()
      data_correct_4 <- list()
      data_incorrect <- list()
      data_incorrect_2 <- list()
      data_incorrect_3 <- list()
      data_incorrect_4 <- list()
      for(ii in seq_along(table.writable$field)){
        field = table.writable$field[ii]
        data_type = table.writable$data_type[ii]

        max_value = table.writable$max_value[ii]

        data_incorrect[[field]] <- dplyr::case_when(
          data_type == "field" ~ "https://not.data.uk",
          data_type == "integer" ~ "-1",
          data_type == "url" ~ "not a url",
          data_type == "choice" ~ "1000",
          data_type == "datetime" ~ "not a date",
          data_type == "boolean" ~ "T"
        )

        data_correct[[field]] <- dplyr::case_when(
          data_type == "field" ~ object_id,
          data_type == "string" ~ "TEST",
          data_type == "integer" ~ "0",
          data_type == "url" ~ "https://test.com",
          data_type == "choice" ~ "0",
          data_type == "datetime" ~ as.character(Sys.time()),
          data_type == "boolean" ~ "TRUE"
        )
        if(data_type == "field" & (grepl(".*?s$", field) |  grepl(".*?s_of$", field))){
          data_correct[[field]] <- list(object_id)
        }

        if(!field %in% table.required$field)
        {
          data_correct_2[[field]] <- ""
          data_correct_3[[field]] <- NULL
          data_incorrect_4[[field]] <- "test"
        }
        else{
          data_correct_2[[field]] <- data_correct[[field]]
          data_correct_3[[field]] <- data_correct[[field]]
        }

        if(data_type == "integer")
        {
          data_correct_4[[field]] <- 1
          data_incorrect_2[[field]] <- Sys.time()
          if(!is.na(max_value))
          {
            data_incorrect_3[[field]] <- max_value + 1
          }
        }
        else if(data_type == "choice"){
          data_correct_4[[field]] <- 0
          data_incorrect_2[[field]] <- Sys.time()
          data_incorrect_3[[field]] <- 1000000000000000000000
        }
        else if(data_type == "datetime"){
          data_correct_4[[field]] <- Sys.time()
          data_incorrect_2[[field]] <- "011110002"
          data_incorrect_3[[field]] <- 1000000000000000000000
        }
        else if(data_type == "boolean"){
          data_correct_4[[field]] <- TRUE
          data_incorrect_2[[field]] <- Sys.time()
          data_incorrect_3[[field]] <- 1000000000000000000000
        }
        else
        {
          data_correct_4[[field]] <- data_correct[[field]]
          data_incorrect_2[[field]] <- data_incorrect[[field]]
          data_incorrect_3[[field]] <- data_incorrect[[field]]
        }
        if(data_type == "string" & !is.na(max_value))
        {
          data_incorrect[[field]] <- paste(rep("t", max_value + 1), collapse = "")
          data_incorrect_2[[field]] <- paste(rep("t", max_value + 1), collapse = "")
          data_incorrect_3[[field]] <- paste(rep("1", max_value + 1), collapse = "")
          data_incorrect_4[[field]] <- paste(rep("1", max_value + 1), collapse = "")
        }

      }
      expect_true(is.list(validate_post_data(table, data_correct, key)))
      Sys.sleep(sleep_time)
      expect_true(is.list(validate_post_data(table, data_correct_2, key)))
      Sys.sleep(sleep_time)
      expect_true(is.list(validate_post_data(table, data_correct_3, key)))
      Sys.sleep(sleep_time)
      expect_true(is.list(validate_post_data(table, data_correct_4, key)))
      Sys.sleep(sleep_time)

      expect_error(validate_post_data(table, data_incorrect, key))
      Sys.sleep(sleep_time)
      expect_error(validate_post_data(table, data_incorrect_2, key))
      Sys.sleep(sleep_time)
      expect_error(validate_post_data(table, data_incorrect_3, key))
      Sys.sleep(sleep_time)
      expect_error(validate_post_data(table, data_incorrect_4, key))


    }}
})
