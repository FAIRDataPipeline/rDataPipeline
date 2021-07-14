context("Testing findme()")

uid <- as.character(random_hash())
version <- "0.1.0"
namespace <- "username"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
endpoint <- Sys.getenv("FDP_endpoint")

# Test writing issues to component ----------------------------------------

data_product <- paste("findme/test/array", uid, sep = "_")
component <- "component/a/s/d/f/s"

# User written config file
config_file <- paste0("config_files/raise_issue/config_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
write_dataproduct(path = config_file,
                  data_product = data_product,
                  description = dataproduct_description,
                  version = version)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = data_product,
                            component = component,
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)))

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "some issue"
severity <- 7
raise_issue(index = component_id,
            handle = handle,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product)
  testthat::expect_equal(handle$issues$use_component, component)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to whole object -------------------------------------

data_product <- paste("findme/test/array2", uid, sep = "_")
component <- "component/a/s/d/f/s"

# User written config file
config_file <- paste0("config_files/raise_issue/config2_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
write_dataproduct(path = config_file,
                  data_product = data_product,
                  description = dataproduct_description,
                  version = version,
                  file_type = "csv")

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product)

write.csv(df, path)

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "another issue"
severity <- 9
raise_issue(handle = handle,
            data_product = data_product,
            version = version,
            namespace = namespace,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product)
  testthat::expect_equal(handle$issues$use_component, NA)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to multiple components by index ---------------------

data_product <- paste("findme/test/array3", uid, sep = "_")
component <- "component/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"

# User written config file
config_file <- paste0("config_files/raise_issue/config3_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
write_dataproduct(path = config_file,
                  data_product = data_product,
                  description = dataproduct_description,
                  version = version)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

component_id1 <- write_array(array = as.matrix(df),
                             handle = handle,
                             data_product = data_product,
                             component = component,
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df),
                                                    colvalue = colnames(df)))

df2 <- data.frame(a = uid, b = uid, c = uid)

component_id2 <- write_array(array = as.matrix(df2),
                             handle = handle,
                             data_product = data_product,
                             component = component2,
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df2),
                                                    colvalue = colnames(df2)))

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple components"
severity <- 7
raise_issue(index = c(component_id1, component_id2),
            handle = handle,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(unique(handle$issues$use_data_product), data_product)
  testthat::expect_equal(handle$issues$use_component[1], component)
  testthat::expect_equal(handle$issues$use_component[2], component2)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

# Test writing issues to multiple components by reference -----------------

data_product <- paste("findme/test/array4", uid, sep = "_")
component <- "component/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"

# User written config file
config_file <- paste0("config_files/raise_issue/config4_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
write_dataproduct(path = config_file,
                  data_product = data_product,
                  description = dataproduct_description,
                  version = version)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product,
            component = component,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)))

df2 <- data.frame(a = uid, b = uid, c = uid)

write_array(array = as.matrix(df2),
            handle = handle,
            data_product = data_product,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df2),
                                   colvalue = colnames(df2)))

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple components"
severity <- 7

raise_issue(handle = handle,
            data_product = data_product,
            component = c(component, component2),
            version = version,
            namespace = namespace,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(unique(handle$issues$use_data_product), data_product)
  testthat::expect_equal(handle$issues$use_component[1], component)
  testthat::expect_equal(handle$issues$use_component[2], component2)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

# Test writing issues to multiple objects by index by reference -----------

data_product <- paste("findme/test/array5-one", uid, sep = "_")
data_product2 <- paste("findme/test/array5-two", uid, sep = "_")

# User written config file
config_file <- paste0("config_files/raise_issue/config5_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
write_dataproduct(path = config_file,
                  data_product = data_product,
                  description = dataproduct_description,
                  version = version,
                  file_type = "csv")
write_dataproduct(path = config_file,
                  data_product = data_product2,
                  description = dataproduct_description,
                  version = version,
                  file_type = "csv")

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
uid <- paste0(uid, 4)
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product)
write.csv(df, path)

uid <- paste0(uid, 5)
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product2)
write.csv(df, path)

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple data products"
severity <- 9
raise_issue(handle = handle,
            data_product = c(data_product, data_product2),
            version = version,
            namespace = namespace,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product[1], data_product)
  testthat::expect_equal(handle$issues$use_data_product[2], data_product2)
  testthat::expect_equal(unique(handle$issues$use_component), NA)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

finalise(handle)

# Test writing issues to multiple objects by index ------------------------

# User written config file
config_file <- paste0("config_files/raise_issue/config6_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
read_dataproduct(path = config_file,
                 data_product = data_product,
                 version = version)
read_dataproduct(path = config_file,
                 data_product = data_product2,
                 version = version)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Read data (write to handle)
path <- link_read(handle, data_product)
path <- link_read(handle, data_product2)

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple data products"
severity <- 9
raise_issue(handle = handle,
            data_product = c(data_product, data_product2),
            version = version,
            namespace = namespace,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product[1], data_product)
  testthat::expect_equal(handle$issues$use_data_product[2], data_product2)
  testthat::expect_equal(unique(handle$issues$use_component), NA)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})
