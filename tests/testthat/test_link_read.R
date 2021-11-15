context("Testing link_read()")

uid <- as.character(random_hash())
data_product1 <- paste("test/csv", uid, sep = "_")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "link_read",
                         paste0("config_", uid, ".yaml"))

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
path <- link_write(handle, data_product1)
df <- data.frame(a = uid, b = uid)
write.csv(df, path)

finalise(handle)


# Run tests ---------------------------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "link_read",
                         paste0("config2_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_read(path = config_file,
         data_product = data_product1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Read data
test_that("function behaves as it should", {
  testthat::expect_true(is.null(handle$inputs))
  path1 <- link_read(handle, data_product1)
  testthat::expect_true(is.character(path1))
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product1)
  path2 <- link_read(handle, data_product1)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(path1, path2)
  tmp <- read.csv(path1)
  testthat::expect_equal(tmp[, -1], df)
})
