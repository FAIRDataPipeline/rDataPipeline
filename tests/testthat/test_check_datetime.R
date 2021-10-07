context("Testing check_datetime()")

endpoint <- Sys.getenv("FDP_endpoint")

tables <- get_tables(endpoint = endpoint)
tables <- setdiff(tables, c("users", "groups"))

fields <- lapply(tables, function(x) get_fields(table = x,
                                                endpoint = endpoint) %>%
                   dplyr::filter(read_only == FALSE,
                                 data_type == "datetime") %>%
                   dplyr::mutate(table = x))
fields <- do.call(rbind.data.frame, fields)

# run_date as POSIXct returns true ----------------------------------------

run_date <- Sys.time()
description <- "test code run"
coderepo_object_url <- new_object(description = "config",
                                  endpoint = endpoint)
config_object_url <- new_object(description = "config",
                                endpoint = endpoint)
script_object_url <- new_object(description = "config",
                                endpoint = endpoint)

test_that("datetime returns true", {
  tmp <- new_code_run(run_date = run_date,
                      description = description,
                      code_repo_url = coderepo_object_url,
                      model_config_url = config_object_url,
                      submission_script_url = script_object_url,
                      inputs_urls = list(),
                      outputs_urls = list(),
                      endpoint = endpoint)
  testthat::expect_true(grepl(endpoint, tmp))

  test <- check_datetime(table = "code_run",
                         this_field = "release_date",
                         query_class = class(run_date),
                         this_query = run_date)
  testthat::expect_true(test)
})

# run_date as POSIXct character returns true -------------------------------

run_date <- as.character(Sys.time())
description <- "test code run"
coderepo_object_url <- new_object(description = "config",
                                  endpoint = endpoint)
config_object_url <- new_object(description = "config",
                                endpoint = endpoint)
script_object_url <- new_object(description = "config",
                                endpoint = endpoint)

test_that("datetime as string returns true", {
  tmp <- new_code_run(run_date = run_date,
                      description = description,
                      code_repo_url = coderepo_object_url,
                      model_config_url = config_object_url,
                      submission_script_url = script_object_url,
                      inputs_urls = list(),
                      outputs_urls = list(),
                      endpoint = endpoint)
  testthat::expect_true(grepl(endpoint, tmp))

  test <- check_datetime(table = "code_run",
                         this_field = "release_date",
                         query_class = class(run_date),
                         this_query = run_date)
  testthat::expect_true(test)
})

# run_date as Date throws error -------------------------------------------

run_date <- Sys.Date()
description <- "test code run"
coderepo_object_url <- new_object(description = "config",
                                  endpoint = endpoint)
config_object_url <- new_object(description = "config",
                                endpoint = endpoint)
script_object_url <- new_object(description = "config",
                                endpoint = endpoint)

test_that("numeric throws error", {
  testthat::expect_error(
    new_code_run(run_date = run_date,
                 description = description,
                 code_repo_url = coderepo_object_url,
                 model_config_url = config_object_url,
                 submission_script_url = script_object_url,
                 inputs_urls = list(),
                 outputs_urls = list(),
                 endpoint = endpoint)
  )

  testthat::expect_error(
    check_datetime(table = "code_run",
                   this_field = "release_date",
                   query_class = class(run_date),
                   this_query = run_date)
  )
})

# run_date as numeric throws error ----------------------------------------

run_date <- 1
description <- "test code run"
coderepo_object_url <- new_object(description = "config",
                                  endpoint = endpoint)
config_object_url <- new_object(description = "config",
                                endpoint = endpoint)
script_object_url <- new_object(description = "config",
                                endpoint = endpoint)

test_that("numeric throws error", {
  testthat::expect_error(
    new_code_run(run_date = run_date,
                 description = description,
                 code_repo_url = coderepo_object_url,
                 model_config_url = config_object_url,
                 submission_script_url = script_object_url,
                 inputs_urls = list(),
                 outputs_urls = list(),
                 endpoint = endpoint)
  )

  testthat::expect_error(
    check_datetime(table = "code_run",
                   this_field = "release_date",
                   query_class = class(run_date),
                   this_query = run_date)
  )
})

# run_date as empty string throws error -----------------------------------

run_date <- ""
description <- "test code run"
coderepo_object_url <- new_object(description = "config",
                                  endpoint = endpoint)
config_object_url <- new_object(description = "config",
                                endpoint = endpoint)
script_object_url <- new_object(description = "config",
                                endpoint = endpoint)

test_that("numeric throws error", {
  testthat::expect_error(
    new_code_run(run_date = run_date,
                 description = description,
                 code_repo_url = coderepo_object_url,
                 model_config_url = config_object_url,
                 submission_script_url = script_object_url,
                 inputs_urls = list(),
                 outputs_urls = list(),
                 endpoint = endpoint)
  )

  testthat::expect_error(
    check_datetime(table = "code_run",
                   this_field = "release_date",
                   query_class = class(run_date),
                   this_query = run_date)
  )
})
