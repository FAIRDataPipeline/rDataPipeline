context("Testing check_datetime()")

endpoint <- Sys.getenv("FDP_endpoint")

tables <- get_tables(endpoint = endpoint)
tables <- setdiff(tables, c("users", "groups"))

fields <- lapply(tables, function(x) get_fields(table = x,
                                                endpoint = endpoint) %>%
                   dplyr::filter(read_only == FALSE,
                                 data_type == "integer") %>%
                   dplyr::mutate(table = x))
fields <- do.call(rbind.data.frame, fields)

# severity as integer returns true ----------------------------------------

object_url <- new_object(description = "")
component_url <- get_entity(object_url)$components

description <- "test issues"
severity <- 7
description <- "an issue"

test_that("integer returns true", {
  tmp <- new_issue(severity = severity,
                   description = description,
                   component_issues = component_url,
                   endpoint = endpoint)
  # testthat::expect_true(grepl(endpoint, tmp))

  # test <- check_integer(table = "issue",
  #                       this_field = "release_date",
  #                       query_class = class(run_date),
  #                       this_query = run_date)
  # testthat::expect_true(test)
})
