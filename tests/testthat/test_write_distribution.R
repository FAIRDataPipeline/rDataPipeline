context("Testing create_distribution()")

uid <- random_hash()
data_product1 <- paste("test/distribution/symptom-delay", uid, sep = "_")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of symptom delay"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "write_distribution",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests ---------------------------------------------------------------

test_that("function behaves as it should", {
  testthat::expect_true(is.null(handle$outputs))
  ind1 <- write_distribution(distribution = "Gaussian",
                            parameters = list(mean = -16.08, SD = 30),
                            handle = handle,
                            data_product = data_product1,
                            component = "symptom-delay",
                            description = "symptom delay")
  testthat::expect_equal(ind1, 1)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  ind2 <- write_distribution(distribution = "Gaussian",
                            parameters = list(mean = -16.08, SD = 30),
                            handle = handle,
                            data_product = data_product1,
                            component = "symptom-delay",
                            description = "symptom delay")
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(ind1, ind2)

  tmp <- handle$outputs %>%
    dplyr::filter(index == ind1)
  path <- tmp$path

  # File should be toml format
  expect_true(configr::is.toml.file(path))
})
