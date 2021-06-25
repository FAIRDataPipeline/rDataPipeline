context("Testing read_array()")

component <- "level/a/s/d/f/s"

# Write test/array v.0.1.0 'username' namespace ---------------------------

config_file <- "config_files/read_array/config.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

df_v1 <- data.frame(a = 1:2, b = 3:4)
rownames(df_v1) <- 1:2

write_array(array = as.matrix(df_v1),
            handle = handle,
            data_product = "test/array",
            component = component,
            description = "test_read_array - original",
            dimension_names = list(rowvalue = rownames(df_v1),
                                   colvalue = colnames(df_v1)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Write test/array v.0.1.0 'username' namespace ---------------------------

config_file <- "config_files/read_array/config2.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

df_v2 <- data.frame(a = 5:6, b = 7:8)
rownames(df_v2) <- 3:4

write_array(array = as.matrix(df_v2),
            handle = handle,
            data_product = "test/array",
            component = component,
            description = "test_read_array - version increment",
            dimension_names = list(rowvalue = rownames(df_v2),
                                   colvalue = colnames(df_v2)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Write test/array v.0.2.0 'johnsmith' namespace --------------------------

config_file <- "config_files/read_array/config3.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

df_js <- data.frame(a = 9:10, b = 11:12)
rownames(df_js) <- 5:6

write_array(array = as.matrix(df_js),
            handle = handle,
            data_product = "test/array",
            component = component,
            description = "test_read_array - different namespace",
            dimension_names = list(rowvalue = rownames(df_js),
                                   colvalue = colnames(df_js)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Write test/array2 v.0.2.0 'username' namespace ---------------------------

config_file <- "config_files/read_array/config4.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

df2 <- data.frame(a = 13:14, b = 15:16)
rownames(df2) <- 7:8

write_array(array = as.matrix(df2),
            handle = handle,
            data_product = "test/array2",
            component = component,
            description = "test_read_array - different dataproduct",
            dimension_names = list(rowvalue = rownames(df2),
                                   colvalue = colnames(df2)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Start tests -------------------------------------------------------------

config_file <- "config_files/read_array/config5.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

component <- "level/a/s/d/f/s"

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "test/array",
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
                    data_product = "test/array2",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_v2)
})

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "test/array3",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df_js)
})

test_that("the correct dataframe is returned", {
  tmp <- read_array(handle = handle,
                    data_product = "test/array4",
                    component = component)
  expect_equivalent(as.data.frame(tmp), df2)
})

finalise(handle)
