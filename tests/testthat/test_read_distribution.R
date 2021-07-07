context("Test read_distribution")

uid <- random_hash()
data_product1 <- paste("test/distribution/symptom-delay", uid, sep = "_")
component1 <- "symptom-delay"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of symptom delay"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- "config_files/read_distribution/config.yaml"
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

# Write data
write_distribution(handle = handle,
                   data_product = data_product1,
                   component = component1,
                   distribution = "Gaussian",
                   parameters = list(mean = -16.08, SD = 30),
                   description = "symptom delay")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

config_file <- "config_files/read_distribution/config2.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
read_dataproduct(path = config_file,
                 data_product = data_product1,
                 component = component1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("function works correctly", {
  tmp <- list(distribution = "Gaussian",
              SD = 30,
              mean = -16.08)
  dist <- read_distribution(handle = handle,
                            data_product = data_product1,
                            component = component1)
  expect_equivalent(dist, tmp)
})
