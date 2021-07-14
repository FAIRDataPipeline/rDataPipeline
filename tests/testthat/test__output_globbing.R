context("Testing output globbing")

# Write to registry -------------------------------------------------------

uid <- as.character(random_hash())
data_product1 <- file.path("real", "data", uid, "1")
data_product2 <- file.path("real", "data", uid, "thing", "1")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/outputglobbing/config_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  file_type = "csv")
write_dataproduct(path = config_file,
                  data_product = data_product2,
                  description = dataproduct_description,
                  file_type = "csv")

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
path1 <- link_write(handle, data_product1, endpoint)
uid1 <- paste0(uid, "_1")
df1 <- data.frame(a = uid1, b = uid1)
write.csv(df1, path1)

path2 <- link_write(handle, data_product2, endpoint)
uid2 <- paste0(uid, "_2")
df2 <- data.frame(a = uid2, b = uid2)
write.csv(df2, path2)

finalise(handle, endpoint)

# Output globbing ---------------------------------------------------------

data_product3 <- file.path("real", "data", uid, "*")
use_version <- "${{MAJOR}}"

# User written config file
config_file <- paste0("config_files/outputglobbing/config2_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product3,
                  description = dataproduct_description,
                  file_type = "csv",
                  use_version = use_version)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

test_that("file1 exists in data store",{
  writes <- handle$yaml$write
  testthat::expect_equal(writes[[1]]$data_product, data_product3)
  testthat::expect_equal(writes[[2]]$data_product, data_product2)
  testthat::expect_equal(writes[[3]]$data_product, data_product1)

  testthat::expect_equal(writes[[1]]$version, "1.0.0")
  testthat::expect_equal(writes[[2]]$version, "1.0.1")
  testthat::expect_equal(writes[[3]]$version, "1.0.1")
})
