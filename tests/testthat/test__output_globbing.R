context("Testing find_write_match()")

# Write to registry -------------------------------------------------------

uid <- as.character(random_hash())
data_product1 <- file.path("data_product", "write", "wildcard", uid, "1")
data_product2 <- file.path("data_product", "write", "wildcard", uid, "1", "2")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "outputglobbing",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          file_type = "csv")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("data products recorded in working config", {
  writes <- handle$yaml$write
  testthat::expect_equal(writes[[1]]$data_product, data_product1)
  testthat::expect_equal(writes[[2]]$data_product, data_product2)

  testthat::expect_equal(writes[[1]]$use$version, "0.0.1")
  testthat::expect_equal(writes[[2]]$use$version, "0.0.1")
})

# Write data
path1 <- link_write(handle, data_product1)
uid1 <- paste0(uid, "_1")
df1 <- data.frame(a = uid1, b = uid1)
write.csv(df1, path1)

path2 <- link_write(handle, data_product2)
uid2 <- paste0(uid, "_2")
df2 <- data.frame(a = uid2, b = uid2)
write.csv(df2, path2)

finalise(handle)

# Output globbing ---------------------------------------------------------

data_product3 <- file.path("data_product", "write", "wildcard", uid, "*")
use_version <- "${{MAJOR}}"

# User written config file
config_file <- file.path(tempdir(), "config_files", "outputglobbing",
                         paste0("config2_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product3,
          description = dataproduct_description,
          file_type = "csv",
          use_version = use_version)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("data products recorded in working config", {
  writes <- handle$yaml$write
  testthat::expect_equal(writes[[1]]$data_product, data_product3)
  testthat::expect_equal(writes[[2]]$data_product, data_product2)
  testthat::expect_equal(writes[[3]]$data_product, data_product1)

  testthat::expect_equal(writes[[1]]$use$version, "1.0.0")
  testthat::expect_equal(writes[[2]]$use$version, "1.0.0")
  testthat::expect_equal(writes[[3]]$use$version, "1.0.0")

  aliases <- find_write_match(handle, data_product3)
  testthat::expect_true(all(aliases %in% c(data_product1, data_product2,
                                           data_product3)))
})

data_product4 <- file.path(dirname(data_product3), "new")

# Write data
path4 <- link_write(handle = handle,
                    data_product = data_product4)
uid4 <- paste0(uid, "_1")
df4 <- data.frame(a = uid4, b = uid4)
write.csv(df4, path4)

path5 <- link_write(handle = handle,
                    data_product = data_product2)
uid5 <- paste0(uid, "_1")
df5 <- data.frame(a = uid5, b = uid5)
write.csv(df5, path5)

test_that("data products recorded in working config", {
  outputs <- handle$outputs
  testthat::expect_equal(outputs$data_product[1], data_product4)
  testthat::expect_equal(outputs$data_product[2], data_product2)

  testthat::expect_equal(outputs$use_version[1], "1.0.0")
  testthat::expect_equal(outputs$use_version[2], "1.0.0")
})
