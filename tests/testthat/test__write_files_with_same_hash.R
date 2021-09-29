context("Testing multiple files with the same hash")

uid <- as.character(random_hash())
data_product1 <- paste("same/hash", uid, sep = "_")
data_product2 <- paste("same/hash2", uid, sep = "_")
component <- "component1/a/s/d/f/s"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
version1 <- "0.1.0"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# Write v0.1.0 of test/array to local registry and data store ---------------

# User written config file
config_file <- paste0("config_files/samehash/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          version = version1,
          file_type = "txt")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          version = version1,
          file_type = "txt")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
cat(uid, file = path1)

path2 <- link_write(handle, data_product2)
cat(uid, file = path2)

finalise(handle)

# Start tests
data_store <- handle$yaml$run_metadata$write_data_store
hash1 <- handle$outputs$hash[1]
file1 <- file.path(paste0(data_store, namespace1), data_product1,
                   paste0(hash1, ".txt"))
hash2 <- handle$outputs$hash[2]
file2 <- file.path(paste0(data_store, namespace1), data_product2,
                   paste0(hash2, ".txt"))

test_that("file1 exists in data store",{
  testthat::expect_true(file.exists(file1))
})

test_that("file2 doesn't exist in data store",{
  testthat::expect_false(file.exists(file2))
})

test_that("handle shows correct path",{
  testthat::expect_true(handle$outputs$path[1] == file1)
  testthat::expect_true(handle$outputs$path[2] == file1)
})

test_that("data registry shows correct path",{
  # File 1
  tmp <- get_entry("storage_location", list(hash = hash1))
  assertthat::assert_that(length(tmp) == 1)
  root <- get_entity(tmp[[1]]$storage_root)$root
  testthat::expect_equal(paste0(root, tmp[[1]]$path),
                         file1)
  # File 2
  tmp <- get_entry("storage_location", list(hash = hash2))
  assertthat::assert_that(length(tmp) == 1)
  root <- get_entity(tmp[[1]]$storage_root)$root
  testthat::expect_equal(paste0(root, tmp[[1]]$path),
                         file1)
})

# -------------------------------------------------------------------------

# User written config file
config_file <- paste0("config_files/samehash2/config_", uid , ".yaml")

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# Will return v.0.1.0, not v.0.2.0
add_read(path = config_file,
         data_product = data_product1,
         use_version = version1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

path <- link_read(handle, data_product1)

file.exists(path)
