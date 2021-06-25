context("Testing new_code_run()")

description <- paste0("test_new_code_run_",
                      openssl::sha1(x = as.character(Sys.time())))
run_date <- Sys.time()

run_server()

code_repo_url <- post_data("object", list(description = description))
code_model_config <- post_data("object", list(description = description))
code_submission_script <- post_data("object", list(description = description))

test_that("new entry in code_run returns API URL",{
  expect_true(
    grepl("code_run", new_code_run(run_date = run_date,
                                   description = description,
                                   code_repo_url = code_repo_url,
                                   model_config_url = code_model_config,
                                   submission_script_url = code_submission_script))
  )
})

test_that("existing entry in code_run returns API URL", {
  expect_true(
    grepl("code_run", new_code_run(run_date = run_date,
                                   description = description,
                                   code_repo_url = code_repo_url,
                                   model_config_url = code_model_config,
                                   submission_script_url = code_submission_script))
  )
})

stop_server()
