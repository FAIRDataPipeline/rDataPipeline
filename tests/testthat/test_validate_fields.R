context("Testing validate_fields()")

endpoint <- Sys.getenv("FDP_endpoint")

#get all tables
tables <- get_tables()
unknown_table <- "unknown"

UID <- paste0("test_validate_fields_ ",
              openssl::sha1(x = as.character(Sys.time())))

object_id <- post_data(table = "object",
                       data = list(description = UID),
                       endpoint = endpoint)


test_that("incorrect tables produce and error", {
  expect_error(validate_fields(table = unknown_table,
                               data = list(),
                               endpoint = endpoint))
  expect_error(validate_fields(table = NULL,
                               data = list(),
                               endpoint = endpoint))
})

test_that("validate_fields works with all tables", {
  for (i in seq_along(tables)) {
    table <- tables[i]

    if (table == "users" | table == "groups") {
      expect_error(validate_fields(table = tables[i],
                                   data = list("username = test"),
                                   endpoint = endpoint))

    } else {
      table.writable <- get_table_writable(table = table,
                                           endpoint = endpoint)
      table.required <- get_table_required(table = table)

      if (nrow(table.required)  > 1)
        expect_error(validate_fields(table = table,
                                     data = list(),
                                     endpoint = endpoint))

      data_correct <- list()
      data_correct_2 <- list()
      data_correct_3 <- list()
      data_correct_4 <- list()
      data_incorrect <- list()
      data_incorrect_2 <- list()
      data_incorrect_3 <- list()
      data_incorrect_4 <- list()

      for (ii in seq_along(table.writable$field)) {
        field <- table.writable$field[ii]
        data_type <- table.writable$data_type[ii]

        max_length <- table.writable$max_length[ii]

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
          data_type == "string" & field == "version" ~ "0.1.0",
          data_type == "string" ~ "TEST",
          data_type == "integer" ~ "0",
          data_type == "url" ~ "https://test.com",
          data_type == "choice" ~ "0",
          data_type == "datetime" ~ as.character(Sys.time()),
          data_type == "boolean" ~ "TRUE")

        if (data_type == "field" & (grepl(".*?s$", field) |
                                    grepl(".*?s_of$", field))) {
          data_correct[[field]] <- list(object_id)
        }

        if (!field %in% table.required$field) {
          data_correct_2[[field]] <- ""
          data_correct_3[[field]] <- NULL
          data_incorrect_4[[field]] <- "test"
        } else {
          data_correct_2[[field]] <- data_correct[[field]]
          data_correct_3[[field]] <- data_correct[[field]]
        }

        if (data_type == "integer") {
          data_correct_4[[field]] <- 1
          data_incorrect_2[[field]] <- Sys.time()
          if (!is.na(max_length)) {
            data_incorrect_3[[field]] <- max_length + 1
          }
        } else if (data_type == "choice") {
          data_correct_4[[field]] <- 0
          data_incorrect_2[[field]] <- Sys.time()
          data_incorrect_3[[field]] <- 1000000000000000000000
        } else if (data_type == "datetime") {
          data_correct_4[[field]] <- Sys.time()
          data_incorrect_2[[field]] <- "011110002"
          data_incorrect_3[[field]] <- 1000000000000000000000
        } else if (data_type == "boolean") {
          data_correct_4[[field]] <- TRUE
          data_incorrect_2[[field]] <- Sys.time()
          data_incorrect_3[[field]] <- 1000000000000000000000
        } else {
          data_correct_4[[field]] <- data_correct[[field]]
          data_incorrect_2[[field]] <- data_incorrect[[field]]
          data_incorrect_3[[field]] <- data_incorrect[[field]]
        }

        if (data_type == "string" & !is.na(max_length)) {
          data_incorrect[[field]] <- paste(rep("t", max_length + 1),
                                           collapse = "")
          data_incorrect_2[[field]] <- paste(rep("t", max_length + 1),
                                             collapse = "")
          data_incorrect_3[[field]] <- paste(rep("1", max_length + 1),
                                             collapse = "")
          data_incorrect_4[[field]] <- paste(rep("1", max_length + 1),
                                             collapse = "")
        }

      }
      expect_true(is.list(validate_fields(table = table,
                                          data = data_correct,
                                          endpoint = endpoint)))
      expect_true(is.list(validate_fields(table = table,
                                          data = data_correct_2,
                                          endpoint = endpoint)))
      expect_true(is.list(validate_fields(table = table,
                                          data = data_correct_3,
                                          endpoint = endpoint)))
      expect_true(is.list(validate_fields(table = table,
                                          data = data_correct_4,
                                          endpoint = endpoint)))

      expect_error(validate_fields(table = table,
                                   data = data_incorrect,
                                   endpoint = endpoint))
      expect_error(validate_fields(table = table,
                                   data = data_incorrect_2,
                                   endpoint = endpoint))
      expect_error(validate_fields(table = table,
                                   data = data_incorrect_3,
                                   endpoint = endpoint))
      expect_error(validate_fields(table = table,
                                   data = data_incorrect_4,
                                   endpoint = endpoint))

    }
  }
})
