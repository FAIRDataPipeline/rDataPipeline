context("Testing read_array()")

uid <- random_hash()
coderun_description <- "Test read_table"
dataproduct_description <- "A test table"
data_product1 <- paste("test/table", uid, sep = "_")
component <- "a/b/c/d"
version1 <- "0.1.0"
namespace1 <- "username"

# Write test/array v.0.1.0 'username' namespace ---------------------------

# User written config file
config_file <- "config_files/read_table/config.yaml"
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
df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

write_table(df = df,
            handle = handle,
            data_product = data_product1,
            component = component1,
            description = "Some description")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

# User written config file
config_file <- "config_files/read_array/config2.yaml"

write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
read_dataproduct(path = config_file,
                 data_product = data_product1,
                 component = component1,
                 use_version = version1)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests
test_that("the correct dataframe is returned", {
  tmp <- read_table(handle = handle,
                    data_product = data_product1,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df)
})
