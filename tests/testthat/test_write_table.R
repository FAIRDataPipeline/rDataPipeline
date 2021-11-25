context("Testing write_table()")

coderun_description <- "Testing write_table()"
dataproduct_description <- "Test table"
data_product1 <- paste("test/table", uid <- random_hash(), sep = "_")
component1 <- "a/b/c/d"
version1 <- "0.1.0"

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
add_write(data_product = data_product1,
          description = dataproduct_description,
          version = version1)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
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
