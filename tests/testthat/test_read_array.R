context("Testing read_array()")

uid <- random_hash()
coderun_description <- "Test read_array"
dataproduct_description <- "A test array"
data_product1 <- paste("test/array", uid, sep = "_")
data_product2 <- paste("test/array2", uid, sep = "_")
data_product3 <- paste("test/array3", uid, sep = "_")
data_product4 <- paste("test/array4", uid, sep = "_")
component <- "a/b/c/d"
version1 <- "0.1.0"
version2 <- "0.2.0"
namespace1 <- "username"
namespace2 <- "johnsmith"

endpoint <- Sys.getenv("FDP_endpoint")

# Write test/array v.0.1.0 'username' namespace ---------------------------

# User written config file
config_file <- "config_files/read_array/config.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
df_v1 <- data.frame(a = 1:2, b = 3:4)
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
finalise(handle, endpoint)

# Write test/array v.0.2.0 'username' namespace ---------------------------

# User written config file
config_file <- "config_files/read_array/config2.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version2)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
df_v2 <- data.frame(a = 5:6, b = 7:8)
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
finalise(handle, endpoint)

# Write test/array v.0.1.0 'johnsmith' namespace --------------------------

# User written config file
config_file <- "config_files/read_array/config3.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace2,
             output_namespace = namespace2)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
df_js <- data.frame(a = 9:10, b = 11:12)
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
finalise(handle, endpoint)

# Write test/array2 v.0.1.0 'username' namespace ---------------------------

# User written config file
config_file <- "config_files/read_array/config4.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product2,
                  description = dataproduct_description,
                  version = version1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

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
finalise(handle, endpoint)

# Start tests -------------------------------------------------------------

# User written config file
config_file <- "config_files/read_array/config5.yaml"

write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)

# Will return v.0.1.0, not v.0.2.0
read_dataproduct(path = config_file,
                 data_product = data_product1,
                 component = component,
                 use_version = version1)

# Will return test/array, but call it test/array2
read_dataproduct(path = config_file,
                 data_product = data_product2,
                 component = component,
                 use_data_product = data_product1)

# Will return test/array in the johnsmith namespace, but call it test/array3
read_dataproduct(path = config_file,
                 data_product = data_product3,
                 component = component,
                 use_data_product = data_product1,
                 use_namespace = namespace2)

# Will return test/array2 v.0.1.0, but call it test/array4
read_dataproduct(path = config_file,
                 data_product = data_product4,
                 component = component,
                 use_data_product = data_product2,
                 use_version = version1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Run tests
test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product1,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_v1)

  expect_equivalent(attributes(tmp)$dimnames[[1]], dimension_names$rowvalue )
  expect_equivalent(attributes(tmp)$dimnames[[2]], dimension_names$colvalue )
  expect_equivalent(attributes(tmp)$Dimension_1_title, names(dimension_names)[1] )
  expect_equivalent(attributes(tmp)$Dimension_2_title, names(dimension_names)[2] )
  expect_equivalent(attributes(tmp)$Dimension_2_units, dimension_units[[2]])
  expect_equivalent(attributes(tmp)$Dimension_2_values, dimension_values[[2]])
  expect_equivalent(attributes(tmp)$units, units)
})

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product2,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_v2)
})

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product3,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_js)
})

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = data_product4,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df2)
})
