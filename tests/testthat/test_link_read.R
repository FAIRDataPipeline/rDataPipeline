context("Testing link_read()")

coderun_description <- "Testing link_read()"
dataproduct_description <- "A csv file"
data_product <- paste("test/csv", random_hash(), sep = "_")

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description,
            file_type = "csv")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
path <- link_write(handle, data_product)
df <- data.frame(a = uid, b = uid)
write.csv(df, path)

finalise(handle)


# Run tests ---------------------------------------------------------------

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_read(data_product = data_product)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Read data
test_that("function behaves as it should", {
  testthat::expect_true(is.null(handle$inputs))
  path1 <- link_read(handle, data_product)
  testthat::expect_true(is.character(path1))
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product)
  path2 <- link_read(handle, data_product)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(path1, path2)
  tmp <- read.csv(path1)
  testthat::expect_equal(tmp[, -1], df)
})
