context("Testing create_distribution()")

coderun_description <- "Testing create_distribution()"
dataproduct_description <- "Estimate of symptom delay"
uid <- random_hash()
data_product <- paste("test/distribution/symptom-delay", uid, sep = "_")

# Generate user-written config file
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = paste0(tempfile(), ".yaml"),
              description = coderun_description,
              script = "echo hello") %>%
add_write(data_product = data_product,
          description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Run tests ---------------------------------------------------------------

test_that("function behaves as it should", {
  testthat::expect_true(is.null(handle$outputs))
  ind1 <- write_distribution(distribution = "Gaussian",
                            parameters = list(mean = -16.08, SD = 30),
                            handle = handle,
                            data_product = data_product,
                            component = "symptom-delay",
                            description = "symptom delay")
  testthat::expect_equal(ind1, 1)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product)
  ind2 <- write_distribution(distribution = "Gaussian",
                            parameters = list(mean = -16.08, SD = 30),
                            handle = handle,
                            data_product = data_product,
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
