context("Testing file_structure()")

##################################################################
##            Create Test Data in test-dir directory            ##
##################################################################
filename <- "test_array_1.h5"
component <- "level1"
df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:nrow(df)
create_array(filename = filename,
             component = component,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))

filename_1 <- "test_array_2.h5"
component_1 <- "level1/level2"
create_array(filename = filename_1,
             component = component_1,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))


filename_2 <- "test_array_3.h5"
component_2 <- "level1/level2/level3"
create_array(filename = filename_2,
             component = component_2,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))

filename_3 <- "test_array_4.h5"
create_array(filename_3,
             component = component,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))
create_array(filename_3,
             component = component_1,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))
create_array(filename_3,
             component = component_2,
             array = as.matrix(df),
             dimension_names = list(rowvalue = rownames(df),
                                    colvalue = colnames(df)))


test_that("an error is thrown if file does not exist", {
  testthat::expect_error(file_structure("unknown_file.h5"))
})

test_that("an h5 file is generated", {
  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
})

test_that("file structures are equal to Component names", {
  testthat::expect_identical(file_structure(filename), component)
  testthat::expect_equal(file_structure(filename_1), component_1)
  testthat::expect_equal(file_structure(filename_2), component_2)
  testthat::expect_equal(file_structure(filename_3),
                         c(component, component_1, component_2))
})


# Remove test file
file.remove(filename)
file.remove(filename_1)
file.remove(filename_2)
file.remove(filename_3)
