context("Testing finalise()")

coderun_description <- "Testing finalise()"
dataproduct_description <- "A nice description"
data_product1 <- file.path("real", "data", random_hash(), "1")

# delete_if_empty ---------------------------------------------------------

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

finalise(handle, delete_if_empty = TRUE)

# delete_if_duplicate -----------------------------------------------------

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

# Write data
path1 <- link_write(handle, data_product1)
df1 <- data.frame(a = uid, b = uid)
write.csv(df1, path1)

finalise(handle)

# Now try to write a duplicate file!

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product1,
            description = dataproduct_description,
            file_type = "csv",
            version = "${{MINOR}}")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
df1 <- data.frame(a = uid, b = uid)
write.csv(df1, path1)

test_that("data products recorded in working config", {
  reads <- handle$outputs
  testthat::expect_equal(reads$data_product, data_product1)
  testthat::expect_equal(reads$use_version, "0.1.0")
})

finalise(handle, delete_if_duplicate = TRUE)

test_that("code run contains no output", {
  coderun_url <- get_entity(handle$code_run)
  testthat::expect_equal(coderun_url$outputs, list())
})
