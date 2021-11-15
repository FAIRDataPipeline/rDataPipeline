context("Testing write_array()")

uid <- random_hash()
namespace1 <- "username"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Try to write two components with the same name"
data_product1 <- paste("test/array", uid, sep = "_")
component1 <- "a/b/c/d"
component2 <- "another/component"
version1 <- "0.1.0"
version2 <- "0.2.0"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "write_array",
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
df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

test_that("incorrect array format throws error", {
  testthat::expect_error(
    write_array(array = df,
                handle = handle,
                data_product = data_product1,
                component = component1,
                description = "Some description",
                dimension_names = list(rowvalue = rownames(df),
                                       colvalue = colnames(df))),
    regexp = "`array` must be an array"
  )
})

test_that("incorrect dimension_names format throws error", {
  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product1,
                component = component1,
                description = "Some description",
                dimension_names = list(rowvalue = data.frame(rownames(df)),
                                       colvalue = colnames(df))),
    regexp = "Elements of dimension_names must be vectors"
  )
})

msg <- paste("Number of elements in dimension_names does not equal number",
             "of dimensions in array")
test_that("incorrect dimension_names length throws error", {
  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product1,
                component = component1,
                description = "Some description",
                dimension_names = list(rowvalue = 1:3,
                                       colvalue = colnames(df))),
    regexp = msg
  )

  msg <- paste("Number of elements in dimension_names does not equal number",
               "of dimensions in array")
  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product1,
                component = component1,
                description = "Some description",
                dimension_names = list(rowvalue = rownames(df),
                                       colvalue = 1)),
    regexp = msg
  )

  msg <- paste("Length of dimension_names does not equal number of",
               "dimensions in array")
  testthat::expect_error(
    msg <-
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product1,
                component = component1,
                description = "Some description",
                dimension_names = list(rowvalue = rownames(df),
                                       colvalue = colnames(df),
                                       othervalue = colnames(df))),
    regexp = msg
  )
})

test_that("entry is recorded in the handle once", {
  testthat::expect_true(is.null(handle$outputs))
  ind1 <- write_array(array = as.matrix(df),
                      handle = handle,
                      data_product = data_product1,
                      component = component1,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
  testthat::expect_equal(ind1, 1)
  testthat::expect_false(is.null(handle$outputs))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(handle$outputs$data_product, data_product1)
  ind2 <- write_array(array = as.matrix(df),
                      handle = handle,
                      data_product = data_product1,
                      component = component1,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
  testthat::expect_equal(nrow(handle$outputs), 1)
  testthat::expect_equal(ind1, ind2)
})

test_that(".h5 file is generated", {
  filename <- handle$outputs %>%
    dplyr::filter(index == index) %>%
    dplyr::select(path) %>%
    unlist() %>%
    unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
})

test_that(".h5 file is generated with unit and dimension values", {
  ind <- write_array(array = as.matrix(df),
                     handle = handle,
                     data_product = data_product1,
                     component = component2,
                     description = "Some description",
                     dimension_names = list(rowvalue = rownames(df),
                                            colvalue = colnames(df)),
                     dimension_values = list(value1 = 1,
                                             value2 = 2),
                     dimension_units = list(unit1 = "day",
                                            unit2 = "year"),
                     units = "days")
  filename <- handle$outputs %>%
    dplyr::filter(index == ind) %>%
    dplyr::select(path) %>%
    unlist() %>%
    unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
  testthat::expect_equal(get_components(filename), c(component1, component2))
})
