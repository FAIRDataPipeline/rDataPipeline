context("Testing create_estimate()")

config_file <- "config_files/write_estimate/config.yaml"
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("function behaves as it should", {

  ind <- write_estimate(value =  192.0,
                        handle = handle,
                        data_product = "test/estimate/asymptomatic-period",
                        component = "asymptomatic-period",
                        description = "asymptomatic period")

  tmp <- handle$outputs %>%
    dplyr::filter(index == ind)
  path <- tmp$path

  # File should be toml format
  expect_true(configr::is.toml.file(path))
})
