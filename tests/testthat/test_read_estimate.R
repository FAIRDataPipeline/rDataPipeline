context("testing read_estimate")

uid <- random_hash()
data_product1 <- paste("test/estimate/asymptomatic-period", uid, sep = "_")
missing_data_product <- paste0("missing_", uid)
component1 <- "asymptomatic-period"
component2 <- "asymptomatic-period2"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Estimate of asymptomatic period"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/read_estimate/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description)
add_write(path = config_file,
          data_product = missing_data_product,
          description = dataproduct_description)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
value1 <- 192.0
write_estimate(value =  value1,
               handle = handle,
               data_product = data_product1,
               component = component1,
               description = "asymptomatic period")

value2 <- 1
write_estimate(value =  value2,
               handle = handle,
               data_product = data_product1,
               component = component2,
               description = "asymptomatic period2")

value3 <- 999999999999999999999999.0123456789
write_estimate(value =  value3,
               handle = handle,
               data_product = missing_data_product,
               component = component1,
               description = "asymptomatic period1")

# Finalise code run
finalise(handle)

# Remove missing_data_product from data store
index <- which(handle$outputs$data_product == missing_data_product)
file.remove(handle$outputs$path[index])

# Start tests -------------------------------------------------------------

config_file <- paste0("config_files/read_estimate/config2_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_read(path = config_file,
         data_product = data_product1,
         component = component1)
add_read(path = config_file,
         data_product = data_product1,
         component = component2)
add_read(path = config_file,
         data_product = missing_data_product,
         component = component1)

fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("error is thrown when file is missing from data store", {
  testthat::expect_error(
    read_estimate(handle = handle,
                  data_product = missing_data_product,
                  component = component1),
    regexp = "File missing from data store"
  )
})

test_that("component1 is returned", {
  dat <- read_estimate(handle = handle,
                       data_product = data_product1,
                       component = component1)

  testthat::expect_equal(dat, value1)
})

test_that("component2 is returned", {
  dat <- read_estimate(handle = handle,
                       data_product = data_product1,
                       component = component2)

  testthat::expect_equal(dat, value2)
})
