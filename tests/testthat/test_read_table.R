context("Testing read_table()")

coderun_description <- "Testing read_table()"
dataproduct_description <- "A test table"
uid <- random_hash()
data_product <- paste("test/table", uid, sep = "_")
component <- "a/b/c/d"
component2 <- "component2"
version1 <- "0.1.0"

# Write test/array v.0.1.0 'username' namespace ---------------------------

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
add_write(data_product = data_product,
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

write_table(df = df,
            handle = handle,
            data_product = data_product,
            component = component,
            description = "Some description")

df2 <- data.frame(a = 5:6, b = 7:8)
rownames(df2) <- 3:4

write_table(df = df2,
            handle = handle,
            data_product = data_product,
            component = component2,
            description = "Some description")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
add_read(data_product = data_product,
         use_version = version1)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Run tests
test_that("df is returned", {
  testthat::expect_true(is.null(handle$inputs))
  tmp1 <- read_table(handle = handle,
                     data_product = data_product,
                     component = component)
  testthat::expect_equivalent(as.data.frame(tmp1), df)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product)
  tmp2 <- read_table(handle = handle,
                     data_product = data_product,
                     component = component)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(tmp1, tmp2)
})

test_that("df2 is returned", {
  tmp <- read_table(handle = handle,
                    data_product = data_product,
                    component = component2)
  expect_equivalent(as.data.frame(tmp), df2)
})
