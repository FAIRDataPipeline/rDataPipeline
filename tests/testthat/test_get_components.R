context("Testing get_components()")

uid <- random_hash()
data_product1 <- paste("test/get_components1", uid, sep = "_")
data_product2 <- paste("test/get_components2", uid, sep = "_")
data_product3 <- paste("test/get_components3", uid, sep = "_")
data_product4 <- paste("test/get_components4", uid, sep = "_")
component1 <- "level1"
component2 <- "level1/level2"
component3 <- "level1/level2/level3"
version1 <- "0.1.0"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "data product description"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "get_components",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          version = version1)
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          version = version1)
add_write(path = config_file,
          data_product = data_product3,
          description = dataproduct_description,
          version = version1)
add_write(path = config_file,
          data_product = data_product4,
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
rownames(df) <- seq_len(nrow(df))
array <- as.matrix(df)

index1 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product1,
                      component = component1,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename1 <- handle$outputs %>%
  dplyr::filter(index == index1) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

index2 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product2,
                      component = component2,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename2 <- handle$outputs %>%
  dplyr::filter(index == index2) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

index3 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product3,
                      component = component3,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename3 <- handle$outputs %>%
  dplyr::filter(index == index3) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

index4 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product4,
                      component = component1,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename4 <- handle$outputs %>%
  dplyr::filter(index == index4) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

index5 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product4,
                      component = component2,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename5 <- handle$outputs %>%
  dplyr::filter(index == index5) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

index6 <- write_array(array = array,
                      handle = handle,
                      data_product = data_product4,
                      component = component3,
                      description = "Some description",
                      dimension_names = list(rowvalue = rownames(df),
                                             colvalue = colnames(df)))
filename6 <- handle$outputs %>%
  dplyr::filter(index == index6) %>%
  dplyr::select(path) %>%
  unlist() %>%
  unname()

test_that("an error is thrown if file does not exist", {
  testthat::expect_error(get_components("unknown_file.h5"),
                         regexp = "File does not exist")
})

test_that("an h5 file is generated", {
  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename1)))
})

test_that("file structures are equal to Component names", {
  testthat::expect_identical(get_components(filename1), component1)
  testthat::expect_equal(get_components(filename2), component2)
  testthat::expect_equal(get_components(filename3), component3)
  testthat::expect_equal(get_components(filename4),
                         c(component1, component2, component3))
})
