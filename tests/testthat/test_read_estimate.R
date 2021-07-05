context("testing read_estimate")

uid <- random_hash()
data_product1 <- paste("test/estimate/asymptomatic-period", uid, sep = "_")
component1 <- "asymptomatic-period"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of asymptomatic period"
namespace1 <- "username"

# User written config file
config_file <- "config_files/read_estimate/config.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
value <- 192.0
write_estimate(value =  value,
               handle = handle,
               data_product = data_product1,
               component = component1,
               description = "asymptomatic period")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

config_file <- "config_files/read_estimate/config2.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
read_dataproduct(path = config_file,
                 data_product = data_product1,
                 component = component1)

fair_pull(config_file)
fair_run(config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("function behaves as it should", {
  dat <- read_estimate(handle = handle,
                       data_product = data_product1,
                       component = component1)

  testthat::expect_equal(dat, value)
})
