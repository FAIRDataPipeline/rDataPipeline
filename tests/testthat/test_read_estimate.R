context("testing read_estimate")

# Write estimate ----------------------------------------------------------

config_file <- "config_files/write_estimate/config.yaml"
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

value <- 192.0

write_estimate(value =  value,
               handle = handle,
               data_product = "test/estimate/asymptomatic-period",
               component = "asymptomatic-period",
               description = "asymptomatic period")

finalise(handle)

# Read estimate -----------------------------------------------------------

config_file <- "config_files/read_estimate/config.yaml"
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("function behaves as it should", {

  dat <- read_estimate(handle = handle,
                       data_product = "test/estimate/asymptomatic-period",
                       component = "asymptomatic-period")

  testthat::expect_equal(dat, value)

})

directory <- handle$yaml$run_metadata$write_data_store
unlink(directory, recursive = TRUE)
