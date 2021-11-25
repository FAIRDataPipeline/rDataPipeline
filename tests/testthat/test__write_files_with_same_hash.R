context("Testing multiple files with the same hash")

coderun_description <- "Testing writing files with the same hash"
dataproduct_description <- "A test array"
uid <- as.character(random_hash())
data_product1 <- paste0("data_product/same_hash/", uid, "/1")
data_product2 <- paste0("data_product/same_hash/", uid, "/2")
component <- "component1/a/s/d/f/s"
version <- "0.1.0"

# Write v0.1.0 of test/array to local registry and data store ---------------

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product1,
            description = dataproduct_description,
            version = version,
            file_type = "txt") %>%
  add_write(data_product = data_product2,
            description = dataproduct_description,
            version = version,
            file_type = "txt")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
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
hash2 <- handle$outputs$hash[2]
namespace <- handle$yaml$run_metadata$default_output_namespace

file1 <- file.path(paste0(data_store, namespace), data_product1,
                   paste0(hash1, ".txt"))
file2 <- file.path(paste0(data_store, namespace), data_product2,
                   paste0(hash2, ".txt"))

test_that("file1 exists in data store", {
  testthat::expect_true(file.exists(file1))
})

test_that("file2 doesn't exist in data store", {
  testthat::expect_false(file.exists(file2))
})

test_that("handle shows correct path", {
  testthat::expect_true(handle$outputs$path[1] == file1)
  testthat::expect_true(handle$outputs$path[2] == file1)
})

test_that("file exists", {
  testthat::expect_true(file.exists(handle$outputs$path[1]))
  testthat::expect_true(file.exists(handle$outputs$path[2]))
})

test_that("data registry shows correct path", {
  # File 1
  tmp <- get_entry("storage_location", list(hash = hash1))
  testthat::expect_equal(length(tmp), 1)
  root <- get_entity(tmp[[1]]$storage_root)$root
  testthat::expect_equal(paste0(root, tmp[[1]]$path),
                         paste0("file://", file1))
  # File 2
  tmp <- get_entry("storage_location", list(hash = hash2))
  testthat::expect_equal(length(tmp), 1)
  root <- get_entity(tmp[[1]]$storage_root)$root
  testthat::expect_equal(paste0(root, tmp[[1]]$path),
                         paste0("file://", file1))
})

# -------------------------------------------------------------------------

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_read(data_product = data_product1) %>%
  add_read(data_product = data_product2)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

path1 <- link_read(handle, data_product1)
path2 <- link_read(handle, data_product2)

test_that("paths are correct", {
  testthat::expect_equal(path1, path2, file1)
})

test_that("file exists", {
  testthat::expect_true(file.exists(path1))
})
