context("Testing write_estimate()")

uid <- random_hash()
data_product1 <- paste("test/estimate/asymptomatic-period", uid, sep = "_")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of asymptomatic period"
namespace1 <- "username"
endpoint <- "https://data.scrc.uk/api/"

# User written config file
config_file <- "config_files/write_estimate/config.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests ---------------------------------------------------------------

test_that("function behaves as it should", {

  ind <- write_estimate(value =  192.0,
                        handle = handle,
                        data_product = data_product1,
                        component = "asymptomatic-period",
                        description = "asymptomatic period")

  tmp <- handle$outputs %>%
    dplyr::filter(index == ind)
  path <- tmp$path

  expect_true(configr::is.toml.file(path))
})
