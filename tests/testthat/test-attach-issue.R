context("Testing attach_issue")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:100, 1, replace=TRUE)

UID <- paste0("issue test ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

external_object_id <- get_entry("external_object", list(updated_by = test_user))[[1]]$object

namespace <- get_entry("namespace", list(updated_by = test_user))[[1]]$url

data_product <- get_entry("data_product", list(updated_by = test_user))[[1]]$url

component <- get_entry("object_component", list(updated_by = test_user))[[1]]$url

severity <- "5"

# test_that("attach isssue works with external_object_doi", {
#   skip_if(is.null(external_object_id))
#   expect_message(expect_true(is.character(attach_issue(UID,
#                                           severity,
#                                           external_object_id,
#                                           key = key
#                                           ))))
# })




