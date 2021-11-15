context("Testing read_array()")

uid <- random_hash()
coderun_description <- "Test read_array"
dataproduct_description <- "A test array"
data_product1 <- paste("test/array", uid, sep = "_")
component <- "a/b/c/d"
version1 <- "0.1.0"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# Write test/array v.0.1.0 'username' namespace ---------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "read_array",
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
df_v1 <- data.frame(a = 1:2, b = 8:9)
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

# Test use block ----------------------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "read_array",
                         paste0("config2_", uid, ".yaml"))

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# Will return v.0.1.0, not v.0.2.0
add_read(path = config_file,
         data_product = data_product1,
         use_version = version1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests
test_that("df_v1 is returned", {
  testthat::expect_true(is.null(handle$inputs))
  tmp1 <- read_array(handle = handle,
                     data_product = data_product1,
                     component = component)
  testthat::expect_equivalent(as.data.frame(tmp1), df_v1)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product1)
  tmp2 <- read_array(handle = handle,
                     data_product = data_product1,
                     component = component)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(tmp1, tmp2)

  testthat::expect_equivalent(attributes(tmp1)$dimnames[[1]],
                              dimension_names$rowvalue)
  testthat::expect_equivalent(attributes(tmp1)$dimnames[[2]],
                              dimension_names$colvalue)
  testthat::expect_equivalent(attributes(tmp1)$Dimension_1_title,
                              names(dimension_names)[1])
  testthat::expect_equivalent(attributes(tmp1)$Dimension_2_title,
                              names(dimension_names)[2])
  testthat::expect_equivalent(attributes(tmp1)$Dimension_2_units,
                              dimension_units[[2]])
  testthat::expect_equivalent(attributes(tmp1)$Dimension_2_values,
                              dimension_values[[2]])
  testthat::expect_equivalent(attributes(tmp1)$units, units)
})
