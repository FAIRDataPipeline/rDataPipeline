context("Testing read_array()")

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2
array <- as.matrix(df)
filename <- "test_array.h5"

dimension_names = list(rowvalue = rownames(df),
                       colvalue = colnames(df))

dimension_values = list(NA, 10)

dimension_units = list(NA, "km")

units = "s"

create_array(filename = filename,
             path = ".",
             component = "level/a/s/d/f/s",
             array = array,
             dimension_names = dimension_names,
             dimension_values = dimension_values,
             dimension_units = dimension_units,
             units = units)

test_that("read_array contains correct data",{
  expect_equivalent(df, as.data.frame(read_array(filepath = filename,
                              component = "level/a/s/d/f/s")))
})

test_that("read_array returns correct attributes", {
  a1 <- read_array(filepath = filename,
                   component = "level/a/s/d/f/s")
  expect_equivalent(attributes(a1)$dimnames[[1]], dimension_names$rowvalue )
  expect_equivalent(attributes(a1)$dimnames[[2]], dimension_names$colvalue )
  expect_equivalent(attributes(a1)$Dimension_1_title, names(dimension_names)[1] )
  expect_equivalent(attributes(a1)$Dimension_2_title, names(dimension_names)[2] )
  expect_equivalent(attributes(a1)$Dimension_2_units, dimension_units[[2]])
  expect_equivalent(attributes(a1)$Dimension_2_values, dimension_values[[2]])
  expect_equivalent(attributes(a1)$units, units)
})

file.remove(filename)
