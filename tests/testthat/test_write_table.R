context("Testing write_array()")

uid <- random_hash()
namespace1 <- "username"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Test table"
data_product1 <- paste("test/table", uid, sep = "_")
component1 <- "a/b/c/d"
version1 <- "0.1.0"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "write_table",
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
df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

test_that(".h5 file is generated", {
  testthat::expect_true(is.null(handle$outputs))
  ind1 <- write_table(df = df,
                      handle = handle,
                      data_product = data_product1,
                      component = component1,
                      description = "Some description")
  testthat::expect_equal(ind1, 1)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  ind2 <- write_table(df = df,
                      handle = handle,
                      data_product = data_product1,
                      component = component1,
                      description = "Some description")
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(ind1, ind2)

  filename <- handle$outputs %>%
    dplyr::filter(index == index) %>%
    dplyr::select(path) %>%
    unlist() %>%
    unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
})
