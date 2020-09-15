context("Test new_coderun")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("coderun ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

code_repo_id <- get_entry("object", list(updated_by = test_user))[[1]]$url
code_model_config <- get_entry("object", list(updated_by = test_user))[[2]]$url
code_submission_script <- get_entry("object", list(updated_by = test_user))[[3]]$url

if(is.null(code_repo_id)){
  code_repo_id <- post_data("object",
                         list(description = UID),
                         key)
}
if(is.null(code_model_config)){
  code_model_config <- post_data("object",
                         list(description = UID),
                         key)
}
if(is.null(code_submission_script)){
  code_submission_script <- post_data("object",
                         list(description = UID),
                         key)
}

description <- paste0("Code Run Test ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)
run_date <- Sys.time()

test_that("New code run returns a character",{
  expect_true(is.character(new_coderun(run_date, description, code_repo_id, code_model_config, code_submission_script, key = key)))
})


###################################################################
##  new_code run does not produce a message if a code run exists  ##
###################################################################

# test_that("If an code_run exists a message is returned", {
#   expect_message(expect_true(is.character(new_coderun(run_date, description, code_repo_id, code_model_config, code_submission_script, key = key))))
# })
