context("Testing write_estimate()")

coderun_description <- "Testing write_estimate()"
dataproduct_description <- "Estimate of asymptomatic period"
uid <- random_hash()
data_product1 <- paste("test/estimate/asymptomatic-period", uid, sep = "_")

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
add_write(data_product = data_product1,
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
  ind1 <- write_estimate(value =  192.0,
                        handle = handle,
                        data_product = data_product1,
                        component = "asymptomatic-period",
                        description = "asymptomatic period")
  testthat::expect_equal(ind1, 1)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  ind2 <- write_estimate(value =  192.0,
                         handle = handle,
                         data_product = data_product1,
                         component = "asymptomatic-period",
                         description = "asymptomatic period")
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(ind1, ind2)

  tmp <- handle$outputs %>%
    dplyr::filter(index == ind1)
  path <- tmp$path

  expect_true(configr::is.toml.file(path))
})
