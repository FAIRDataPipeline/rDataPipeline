context("Allow multiple versions to be read/written from the same config")

# Write to registry -------------------------------------------------------

uid <- as.character(random_hash())
data_product1 <- file.path("output", "data", paste0(uid, "_1"))
data_product2 <- file.path("output", "data", paste0(uid, "_2"))
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")
if (grepl("localhost", endpoint)) run_server()

# User written config file
config_file <- paste0("config_files/multiversion/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv",
          version = "0.0.1")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          file_type = "csv",
          use_data_product = data_product1,
          use_version = "0.0.2")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")

handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)
path1 <- link_write(handle, data_product1)
write.csv(df, path1)

uid2 <- paste0(uid, "_2")
df <- data.frame(a = uid2, b = uid2)
path2 <- link_write(handle, data_product2)
write.csv(df, path2)

test_that("data products recorded in working config",{
  writes <- handle$outputs
  testthat::expect_equal(writes$data_product[1], data_product1)
  testthat::expect_equal(writes$data_product[2], data_product2)

  testthat::expect_equal(writes$use_data_product[1], data_product1)
  testthat::expect_equal(writes$use_data_product[2], data_product1)

  testthat::expect_equal(writes$use_version[1], "0.0.1")
  testthat::expect_equal(writes$use_version[2], "0.0.2")
})

finalise(handle)



test_that("data products recorded in working config",{
  versions <- c("0.0.1", "0.0.2")
  for (i in 1:2) {
    output_component_url <- get_entity(handle$code_run)$outputs[[i]]
    output_object_url <- get_entity(output_component_url)$object
    output_dp_url <- get_entity(output_object_url)$data_product

    testthat::expect_equal(get_entity(output_dp_url)$name, data_product1)
    testthat::expect_equal(get_entity(output_dp_url)$version, versions[i])
  }
})

