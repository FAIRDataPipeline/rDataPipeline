context("Testing that rDataPipeline works with pipes")

coderun_description <- "Testing that rDataPipeline works with pipes"
uid <- random_hash()

# Test write_array() -----------------------------------------------------

dataproduct_description <- "Test data"
data_product <- paste("test/array", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

array <- matrix(1, 2, 2)

testthat::test_that("read_array() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  handle %>% write_array(array = array,
                         handle = .,
                         data_product = data_product,
                         component = "component",
                         description = "component description")
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(handle$outputs$data_product, data_product)
})

handle %>% finalise()

# Test read_array() -------------------------------------------------------

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
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

testthat::test_that("read_array() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  dat <- handle %>% read_array(data_product = data_product,
                               component = "component")
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(handle$inputs$data_product, data_product)
})

handle %>% finalise()

# Test write_table() -----------------------------------------------------

dataproduct_description <- "Test data"
data_product <- paste("test/table", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

table <- data.frame(1, 2, 2)

testthat::test_that("write_array() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  handle %>% write_table(df = table,
                         handle = .,
                         data_product = data_product,
                         component = "component",
                         description = "component description")
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(handle$outputs$data_product, data_product)
  handle %>% finalise()
})

# Test read_table() -------------------------------------------------------

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
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

testthat::test_that("read_table() works with pipes", {
  testthat::expect_true(is.null(handle$inputs))
  dat <- handle %>% read_table(data_product = data_product,
                               component = "component")
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(handle$inputs$data_product, data_product)
  handle %>% finalise()
})

# Test link_write() -------------------------------------------------------

data_product <- paste("test/link", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description,
            file_type = "txt")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

testthat::test_that("link_write() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  path <- handle %>% link_write(data_product)
  cat("text\n", file = path)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(handle$outputs$data_product, data_product)
  handle %>% finalise()
})

# Test link_read() --------------------------------------------------------

data_product <- paste("test/link", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
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

testthat::test_that("link_read() works with pipes", {
  testthat::expect_true(is.null(handle$inputs))
  path <- handle %>% link_read(data_product)
  dat <- read.table(path)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(handle$inputs$data_product, data_product)
  handle %>% finalise()
})

# Test write_estimate() ---------------------------------------------------

data_product <- paste("test/estimate", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

testthat::test_that("write_estimate() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  dat <- handle %>% write_estimate(value = 6,
                                   handle = .,
                                   data_product = data_product,
                                   component = "component",
                                   description = "component description")
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(handle$outputs$data_product, data_product)
  handle %>% finalise()
})

# Test read_estimate() ----------------------------------------------------

data_product <- paste("test/estimate", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
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

testthat::test_that("read_estimate() works with pipes", {
  testthat::expect_true(is.null(handle$inputs))
  dat <- handle %>% read_estimate(data_product = data_product,
                                  component = "component")
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(handle$inputs$data_product, data_product)
  handle %>% finalise()
})

# Test write_distribution() -----------------------------------------------

data_product <- paste("test/distribution", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

testthat::test_that("write_distribution() works with pipes", {
  testthat::expect_true(is.null(handle$outputs))
  dat <- handle %>% write_distribution(distribution = "Gaussian",
                                       parameters = list(mean = -16.08,
                                                         SD = 30),
                                       handle = .,
                                       data_product = data_product,
                                       component = "component",
                                       description = "component description")
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(handle$outputs$data_product, data_product)
  handle %>% finalise()
})

# Test read_distribution() ------------------------------------------------

data_product <- paste("test/distribution", uid, sep = "_")

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
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

testthat::test_that("read_distribution() works with pipes", {
  testthat::expect_true(is.null(handle$inputs))
  dat <- handle %>% read_distribution(data_product = data_product,
                                      component = "component")
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(handle$inputs$data_product, data_product)
  handle %>% finalise()
})
