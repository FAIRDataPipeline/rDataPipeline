context("Testing link_write()")

uid <- random_hash()
data_product1 <- paste("test/csv", uid, sep = "_")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "link_write",
                         paste0("config_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests ---------------------------------------------------------------

test_that("entry is recorded in the handle once", {
  testthat::expect_true(is.null(handle$outputs))
  path1 <- link_write(handle, data_product1)
  testthat::expect_true(is.character(path1))
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  path2 <- link_write(handle, data_product1)
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(path1, path2)
  tmp <- file.path(namespace1, data_product1)
  testthat::expect_true(grepl(tmp, path1))
})
