context("Testing findme()")

uid <- as.character(random_hash())
data_product1 <- paste("findme/test/array", uid, sep = "_")
component1 <- "component1/a/s/d/f/s"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
version1 <- "0.1.0"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# Test writing issues to component ----------------------------------------

# User written config file
config_file <- "config_files/raise_issue/config1.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version1)

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
                            data_product = data_product1,
                            component = component1,
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)),
                            dimension_values = list(NA, 10),
                            dimension_units = list(NA, "km"),
                            units = "s")

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
  testthat::expect_equal(handle$issues$use_data_product, data_product1)
  testthat::expect_equal(handle$issues$use_component, component1)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to whole object -------------------------------------

# User written config file
config_file <- "config_files/raise_issue/config2.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version1,
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
path <- link_write(handle, data_product1)

write.csv(df, path)

test_that("handle issues is null",{
  testthat::expect_true(is.null(handle$issues))
})

issue <- "another issue"
severity <- 9
raise_issue(handle = handle,
            data_product = data_product1,
            version = version1,
            namespace = namespace1,
            issue = issue,
            severity = severity)

test_that("handle contains issues block",{
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product1)
  testthat::expect_equal(handle$issues$use_component, NA)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})
