context("testing read_estimate")

coderun_description <- "Testing read_estimate()"
dataproduct_description <- "Estimate of asymptomatic period"
uid <- random_hash()
data_product <- paste("test/estimate/asymptomatic-period", uid, sep = "_")
missing_data_product <- paste0("missing_", uid)
component1 <- "asymptomatic-period"
component2 <- "asymptomatic-period2"

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description) %>%
  add_write(data_product = missing_data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
values <- sample(1:1E9, 3, FALSE)
value1 <- values[1]
write_estimate(value =  value1,
               handle = handle,
               data_product = data_product,
               component = component1,
               description = "asymptomatic period")

value2 <- values[2]
write_estimate(value =  value2,
               handle = handle,
               data_product = data_product,
               component = component2,
               description = "asymptomatic period2")

value3 <- values[3]
write_estimate(value =  value3,
               handle = handle,
               data_product = missing_data_product,
               component = component1,
               description = "asymptomatic period1")

# Finalise code run
finalise(handle)

# Remove missing_data_product from data store
index <- which(handle$outputs$data_product == missing_data_product)
file.remove(handle$outputs$path[index])

# Start tests -------------------------------------------------------------

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_read(data_product = data_product) %>%
  add_read(data_product = missing_data_product)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

test_that("error is thrown when file is missing from data store", {
  testthat::expect_error(
    read_estimate(handle = handle,
                  data_product = missing_data_product,
                  component = component1),
    regexp = "File missing from data store"
  )
})

test_that("component1 is returned", {
  testthat::expect_true(is.null(handle$inputs))
  dat1 <- read_estimate(handle = handle,
                        data_product = data_product,
                        component = component1)
  testthat::expect_equal(dat1, value1)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product)
  dat2 <- read_estimate(handle = handle,
                        data_product = data_product,
                        component = component1)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(dat1, dat2)
})

test_that("component2 is returned", {
  dat <- read_estimate(handle = handle,
                       data_product = data_product,
                       component = component2)

  testthat::expect_equal(dat, value2)
})
