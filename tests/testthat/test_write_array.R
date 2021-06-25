context("Testing write_array()")

run_server()

config_file <- "write_array/config.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

data_product <- "test/array"
component <- "this_component"
component2 <- "another_component"

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

test_that("incorrect array format throws error", {
  testthat::expect_error(
    write_array(array = df,
                handle = handle,
                data_product = data_product,
                component = component,
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
                data_product = data_product,
                component = component,
                description = "Some description",
                dimension_names = list(rowvalue = data.frame(rownames(df)),
                                       colvalue = colnames(df))),
    regexp = "Elements of dimension_names must be vectors"
  )
})

test_that("incorrect dimension_names length throws error", {
  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product,
                component = component,
                description = "Some description",
                dimension_names = list(rowvalue = 1:3,
                                       colvalue = colnames(df))),
    regexp = "Number of elements in dimension_names does not equal number of dimensions in array"
  )

  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product,
                component = component,
                description = "Some description",
                dimension_names = list(rowvalue = rownames(df),
                                       colvalue = 1)),
    regexp = "Number of elements in dimension_names does not equal number of dimensions in array"
  )

  testthat::expect_error(
    write_array(array = as.matrix(df),
                handle = handle,
                data_product = data_product,
                component = component,
                description = "Some description",
                dimension_names = list(rowvalue = rownames(df),
                                       colvalue = colnames(df),
                                       othervalue = colnames(df))),
    regexp = "Length of dimension_names does not equal number of dimensions in array"
  )
})

test_that(".h5 file is generated", {
  ind <- write_array(array = as.matrix(df),
                       handle = handle,
                       data_product = data_product,
                       component = component,
                       description = "Some description",
                       dimension_names = list(rowvalue = rownames(df),
                                              colvalue = colnames(df)))
  filename <- handle$outputs %>%
    dplyr::filter(index == index) %>%
    dplyr::select(path) %>%
    unlist() %>% unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
})

test_that(".h5 file is generated with unit and dimension values", {
  ind <- write_array(array = as.matrix(df),
                       handle = handle,
                       data_product = data_product,
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
    unlist() %>% unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))

  file.h5 <- rhdf5::H5Fopen(filename)
  tmp <- unique(h5ls(file.h5)$group)[-1]
  tmp <- gsub("/", "", tmp)

  rhdf5::h5closeAll()
  testthat::expect_equal(tmp, c(component2, component))
})

directory <- handle$yaml$run_metadata$default_data_store
unlink(directory, recursive = TRUE)
