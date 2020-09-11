context("Testing attach_issue")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

UID <- paste0("issue_test_", format(Sys.time(), "%d%m%y%H%M%S"))

external_object_id <- get_entry("external_object", list(updated_by = test_user))[[1]]$url

external_object_id_doi <- NULL
if(!is.null(external_object_id)) {
  external_object_id_doi <- get_entity("external_object", basename(external_object_id))$doi_or_unique_name
}

namespace <- get_entry("namespace", list(updated_by = test_user))[[1]]$url

data_product <- get_entry("data_product", list(updated_by = test_user))[[1]]$url

component <- get_entry("object_component", list(updated_by = test_user))[[1]]$url

severity <- "5"

# test_that("attach isssue works with external_object_doi", {
#   skip_if(is.null(external_object_id_doi))
#   expect_message(expect_true(is.character(attach_issue(UID,
#                                           severity,
#                                           external_object_id,
#                                           key = key
#                                           ))))
# })




