context("Testing create_distribution()")

config_file <- "config_files/write_distribution/config.yaml"
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("function behaves as it should", {

  ind <- write_distribution(handle = handle,
                            data_product = "test/distribution/symptom-delay",
                            component = "symptom-delay",
                            distribution = "Gaussian",
                            parameters = list(mean = -16.08, SD = 30),
                            description = "symptom delay")

  tmp <- handle$outputs %>%
    dplyr::filter(index == ind)
  path <- tmp$path

  # File should be toml format
  expect_true(configr::is.toml.file(path))
})

directory <- handle$yaml$run_metadata$write_data_store
unlink(directory, recursive = TRUE)
