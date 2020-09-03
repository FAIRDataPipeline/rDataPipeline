context("test register_everything")

token <- Sys.getenv("SCRC_API_TOKEN")

date <- Sys.time()
namespace <- "TEST"
name <- paste0(namespace, "_", format(date, "%d%m%y%H%M%S"))
submission_script <- paste0(name, ".r")
original_source_name <- name
original_source_Id <- namespace
original_root <- namespace
original_path <- paste0("data-raw/", name)

# test_that("register everything errors with incorrect submission script", {
#   expect_error(register_everything(name,
#                                     create_version_number(),
#                                     name,
#                                     namespace,
#                                     submission_script,
#                                     original_source_name,
#                                     original_source_Id,
#                                     original_root,
#                                     original_path,
#                                     token))
# })
#
# test_that("register everything errors with incorrect submission script", {
#   expect_silent(register_everything(name,
#                                    create_version_number(),
#                                    name,
#                                    namespace,
#                                    "issues.R",
#                                    original_source_name,
#                                    original_source_Id,
#                                    original_root,
#                                    original_path,
#                                    token))
# })


