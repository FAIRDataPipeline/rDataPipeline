context("Test read_distribution")

uid <- random_hash()
data_product1 <- paste("test/distribution/symptom-delay", uid, sep = "_")
component1 <- "symptom-delay"
component2 <- "symptom-delay2"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of symptom delay"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "read_distribution",
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

# Write data
dat1 <- list(distribution = "Gaussian",
             parameters = list(mean = -16.08, SD = 30))

write_distribution(handle = handle,
                   data_product = data_product1,
                   component = component1,
                   distribution = dat1$distribution,
                   parameters = dat1$parameters,
                   description = "symptom delay")

dat2 <- list(distribution = "Gaussian",
             parameters = list(mean = 1, SD = 3))

write_distribution(handle = handle,
                   data_product = data_product1,
                   component = component2,
                   distribution = dat2$distribution,
                   parameters = dat2$parameters,
                   description = "symptom delay")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

config_file <- file.path(tempdir(), "config_files", "read_distribution",
                         paste0("config2_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_read(path = config_file,
         data_product = data_product1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

tmp <- list(distribution = dat1$distribution,
            SD = dat1$parameters$SD,
            mean = dat1$parameters$mean)

test_that("function works correctly", {
  testthat::expect_true(is.null(handle$inputs))
  dist1 <- read_distribution(handle = handle,
                            data_product = data_product1,
                            component = component1)
  testthat::expect_equivalent(dist1, tmp)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product1)
  dist2 <- read_distribution(handle = handle,
                             data_product = data_product1,
                             component = component1)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(dist1, dist2)
})
