context("Testing use block")

coderun_description <- "Testing use block"
dataproduct_description <- "A test array"
uid <- random_hash()
data_product1 <- paste("test/array", uid, sep = "_")
data_product2 <- paste("test/array2", uid, sep = "_")
component <- "a/b/c/d"
component2 <- "component2"
version1 <- "0.1.0"
version2 <- "0.2.0"
namespace <- "johnsmith"

# Write test/array v.0.1.0 default namespace -------------------------------

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
df_v1 <- data.frame(a = sample(2000,2), b = sample(2000,2))
rownames(df_v1) <- 1:2
dimension_names <- list(rowvalue = rownames(df_v1),
                        colvalue = colnames(df_v1))
dimension_values <- list(NA, 10)
dimension_units <- list(NA, "km")
units <- "s"

write_array(array = as.matrix(df_v1),
            handle = handle,
            data_product = data_product1,
            component = component,
            description = "test_read_array - original",
            dimension_names = dimension_names,
            dimension_values = dimension_values,
            dimension_units = dimension_units,
            units = units)

# Finalise code run
finalise(handle)

# Write test/array v.0.2.0 default namespace -------------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product1,
            description = dataproduct_description,
            version = version2)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
df_v2 <- data.frame(a = sample(2000,2), b = sample(2000,2))
rownames(df_v2) <- 3:4

write_array(array = as.matrix(df_v2),
            handle = handle,
            data_product = data_product1,
            component = component,
            description = "test_read_array - version increment",
            dimension_names = list(rowvalue = rownames(df_v2),
                                   colvalue = colnames(df_v2)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

# Finalise code run
finalise(handle)

# Write test/array v.0.1.0 'johnsmith' namespace --------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product1,
            description = dataproduct_description,
            version = version1,
            use_namespace = namespace)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
df_js <- data.frame(a = sample(2000,2), b = sample(2000,2))
rownames(df_js) <- 5:6

write_array(array = as.matrix(df_js),
            handle = handle,
            data_product = data_product1,
            component = component,
            description = "test_read_array - different namespace",
            dimension_names = list(rowvalue = rownames(df_js),
                                   colvalue = colnames(df_js)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

# Finalise code run
finalise(handle)

# Write test/array2 v.0.1.0 default namespace ------------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product2,
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
df2 <- data.frame(a = 13:14, b = 15:16)
rownames(df2) <- 7:8

write_array(array = as.matrix(df2),
            handle = handle,
            data_product = data_product2,
            component = component,
            description = "test_read_array - different dataproduct",
            dimension_names = list(rowvalue = rownames(df2),
                                   colvalue = colnames(df2)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

# Finalise code run
finalise(handle)

# Multiple components -----------------------------------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product5,
            description = dataproduct_description,
            version = version1)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

comp1 <- data.frame(a = 17:18, b = 19:20)
rownames(comp1) <- 9:10

write_array(array = as.matrix(comp1),
            handle = handle,
            data_product = data_product5,
            component = component,
            description = "component1",
            dimension_names = dimension_names)

comp2 <- data.frame(a = 21:22, b = 23:24)
rownames(comp2) <- 11:12

write_array(array = as.matrix(comp2),
            handle = handle,
            data_product = data_product5,
            component = component2,
            description = "component2",
            dimension_names = dimension_names)

# Finalise code run
finalise(handle)

# Test use block ----------------------------------------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  # Will return test/array v.0.1.0 default namespace
  add_read(data_product = "one",
           use_data_product = data_product1,
           use_version = version1) %>%
  # Will return test/array v.0.2.0 default namespace
  add_read(data_product = "two",
           use_data_product = data_product1) %>%
  # Will return test/array v.0.1.0 'johnsmith' namespace
  add_read(data_product = "three",
           use_data_product = data_product1,
           use_namespace = namespace) %>%
  # Will return test/array2 v.0.1.0 default namespace
  add_read(data_product = "four",
           use_data_product = data_product2)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Run tests
test_that("df_v1 is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "one",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_v1)

  expect_equivalent(attributes(tmp)$dimnames[[1]], dimension_names$rowvalue)
  expect_equivalent(attributes(tmp)$dimnames[[2]], dimension_names$colvalue)
  expect_equivalent(attributes(tmp)$Dimension_1_title,
                    names(dimension_names)[1])
  expect_equivalent(attributes(tmp)$Dimension_2_title,
                    names(dimension_names)[2])
  expect_equivalent(attributes(tmp)$Dimension_2_units, dimension_units[[2]])
  expect_equivalent(attributes(tmp)$Dimension_2_values, dimension_values[[2]])
  expect_equivalent(attributes(tmp)$units, units)
})

test_that("df_v2 is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "two",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_v2)
})

test_that("df_js is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "three",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_js)
})

test_that("df2 is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "four",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df2)
})

# Test multiple components ------------------------------------------------

config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  # Will return v.0.1.0, not v.0.2.0
  add_read(data_product = data_product5,
           use_version = version1)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

test_that("component1 is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product5,
                    component = component)
  expect_equivalent(as.data.frame(tmp), comp1)
})

test_that("component2 is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product5,
                    component = component2)
  expect_equivalent(as.data.frame(tmp), comp2)
})
