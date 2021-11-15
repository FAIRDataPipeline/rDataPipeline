context("Testing finalise()")

uid <- as.character(random_hash())
data_product1 <- file.path("real", "data", uid, "1")
dataproduct_description <- "a nice description"
coderun_description <- "Do nothing"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# delete_if_empty ---------------------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "finalise",
                         paste0("config_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle, delete_if_empty = TRUE)

# delete_if_duplicate -----------------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "finalise",
                         paste0("config2_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
df1 <- data.frame(a = uid, b = uid)
write.csv(df1, path1)

finalise(handle)

# Now try to write a duplicate file!

# User written config file
config_file <- file.path(tempdir(), "config_files", "finalise",
                         paste0("config3_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv",
          version = "${{MINOR}}")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
df1 <- data.frame(a = uid, b = uid)
write.csv(df1, path1)

test_that("data products recorded in working config", {
  reads <- handle$outputs
  testthat::expect_equal(reads$data_product, data_product1)
  testthat::expect_equal(reads$use_version, "0.1.0")
})

finalise(handle, delete_if_duplicate = TRUE)

test_that("code run contains no output", {
  coderun_url <- get_entity(handle$code_run)
  testthat::expect_equal(coderun_url$outputs, list())
})
