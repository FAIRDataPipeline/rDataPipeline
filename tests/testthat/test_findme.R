context("Testing findme()")

uid <- as.character(random_hash())
data_product1 <- paste("findme/test/array", uid, sep = "_")
data_product2 <- paste("findme/test/array2", uid, sep = "_")
component1 <- "component1/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
version1 <- "0.1.0"
version2 <- "0.2.0"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# Write v0.1.0 of test/array to local registry and data store ---------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "findme",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          version = version1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component1,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Start tests
file <- unique(handle$outputs$path)

test_that("findme prints output", {
  tmp <- testthat::capture_output_lines(findme(file = file,
                                               endpoint = endpoint))
  testthat::expect_true(grepl("hash", tmp[1]))
  testthat::expect_true(grepl("location", tmp[3]))
  testthat::expect_true(grepl("data product", tmp[5]))
})

# Write v0.2.0 of test/array to local registry and data store ---------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "findme",
                         paste0("config2_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          version = version2)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component1,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Start tests
file <- unique(handle$outputs$path)

test_that("findme lists two data products", {
  tmp <- testthat::capture_output_lines(findme(file = file,
                                               endpoint = endpoint))
  testthat::expect_true(grepl(data_product1, tmp[6]))
  testthat::expect_true(grepl(data_product1, tmp[17]))
})

# Write v0.1.0 of test/array2 to local registry and data store --------------

uid <- as.character(random_hash())
data_product3 <- paste("findme/test/array2", uid, sep = "_")

# User written config file
config_file <- file.path(tempdir(), "config_files", "findme",
                         paste0("config3_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product3,
          description = dataproduct_description,
          version = version1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

# Write data
component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = data_product3,
                            component = component1,
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)),
                            dimension_values = list(NA, 10),
                            dimension_units = list(NA, "km"),
                            units = "s")

raise_issue(index = component_id,
            handle = handle,
            issue = "some issue",
            severity = 7)

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product3,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Start tests
file <- unique(handle$outputs$path)

test_that("findme returns TRUE", {
  tmp <- findme(file = file, endpoint = endpoint)
  testthat::expect_true(tmp)
})
