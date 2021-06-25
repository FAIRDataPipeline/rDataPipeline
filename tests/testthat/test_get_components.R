context("Testing get_components()")

config_file <- "write_array/config.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

# Create Test Data in test-dir directory

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:nrow(df)
array <- as.matrix(df)

data_product1 <- "test/get_components1"
component1 <- "level1"
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
  unlist() %>% unname()

data_product2 <- "test/get_components2"
component2 <- "level1/level2"
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
  unlist() %>% unname()

data_product3 <- "test/get_components3"
component3 <- "level1/level2/level3"
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
  unlist() %>% unname()

data_product4 <- "test/get_components4"
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
  unlist() %>% unname()

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
  unlist() %>% unname()

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
  unlist() %>% unname()

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

# Remove test datastore
directory <- handle$yaml$run_metadata$default_data_store
unlink(directory, recursive = TRUE)
