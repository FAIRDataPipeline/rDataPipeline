context("Testing post_data()")

# Get all tables
tables <- get_tables()
unknown_table <- "unknown"
id <- sample(1:100, 1, replace = TRUE)
uid <- paste0("Test - ", Sys.time())

endpoint <- Sys.getenv("FDP_endpoint")

object_id <- post_data("object", list(description = uid), endpoint = endpoint)

test_that("incorrect tables produce and error", {
  expect_error(post_data(unknown_table, data = list(), endpoint = endpoint))
  expect_error(post_data(NULL, data = list(), endpoint = endpoint))
})

test_that("post_data works with all tables", {
  for (i in seq_along(tables)) {
    table <- tables[i]

    if (table == "users" | table == "groups") {
      expect_error(post_data(table = tables[i],
                             data = list("username = test"),
                             endpoint = endpoint))

    } else {
      table.writable <- get_table_writable(table = table, endpoint = endpoint)
      table.required <- get_table_required(table = table)

      data_correct <- list()
      data_incorrect <- list()

      if (nrow(table.required) > 1) {
        test_that(paste0(table, " fails when no data is present"), {
          expect_error(post_data(table = table,
                                 data = list(),
                                 endpoint = endpoint))
        })

      } else if (nrow(table.required) == 0) {
        # test_that(paste0(table, " allows creation with no data"), {
        #   expect_true(is.character(post_data(table = table,
        #                                      data = NULL,
        #                                      endpoint = endpoint)))
        # })
        # data_incorrect <- list(unknown = "unknown")
        next

      } else {

        for (ii in seq_along(table.required$field)) {
          field <- table.required$field[ii]
          data_type <- table.required$data_type[ii]

          max_value <- table.required$max_value[ii]

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
            data_type == "string" ~ uid,
            data_type == "integer" ~ as.character(id),
            data_type == "url" ~ paste0("https://test.com/", uid),
            data_type == "choice" ~ "0",
            data_type == "datetime" ~ as.character(Sys.time()),
            data_type == "boolean" ~ "TRUE"
          )
          if (data_type == "field" & (grepl(".*?s$", field) |
                                      grepl(".*?s_of$", field))) {
            data_correct[[field]] <- NULL
          }

          if (data_type == "string" & !is.null(max_value)) {
            data_incorrect[[field]] <- paste(rep("t", max_value + 1),
                                             collapse = "")
          }
        }

        test_that(paste0("table ", table, " works with correct data"), {
          expect_true(is.character(post_data(table = table,
                                             data = data_correct,
                                             endpoint = endpoint)))
        })

      }

      if (table != "object")
        test_that(paste0("table ", table,
                         " does not works with correct data"), {
                           expect_error(post_data(table = table,
                                                  data = data_incorrect,
                                                  endpoint = endpoint))
                         })
    }
  }
})
