context("Testing link_write()")

coderun_description <- "Testing link_write()"
dataproduct_description <- "A csv file"
data_product1 <- paste("test/csv", random_hash(), sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product1,
            description = dataproduct_description,
            file_type = "csv")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Run tests ---------------------------------------------------------------

test_that("entry is recorded in the handle once", {
  testthat::expect_true(is.null(handle$outputs))
  path1 <- link_write(handle, data_product1)
  testthat::expect_true(is.character(path1))
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  path2 <- link_write(handle, data_product1)
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(path1, path2)
  namespace <- handle$yaml$run_metadata$default_output_namespace
  tmp <- file.path(namespace, data_product1)
  testthat::expect_true(grepl(tmp, path1))
})
