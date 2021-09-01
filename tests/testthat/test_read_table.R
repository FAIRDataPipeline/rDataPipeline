context("Testing read_table()")

uid <- random_hash()
coderun_description <- "Test read_table"
dataproduct_description <- "A test table"
data_product1 <- paste("test/table", uid, sep = "_")
component <- "a/b/c/d"
component2 <- "component2"
version1 <- "0.1.0"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# Write test/array v.0.1.0 'username' namespace ---------------------------

# User written config file
config_file <- paste0("config_files/read_table/config_", uid , ".yaml")
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

write_table(df = df,
            handle = handle,
            data_product = data_product1,
            component = component,
            description = "Some description")

df2 <- data.frame(a = 5:6, b = 7:8)
rownames(df2) <- 3:4

write_table(df = df2,
            handle = handle,
            data_product = data_product1,
            component = component2,
            description = "Some description")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

# User written config file
config_file <- paste0("config_files/read_table/config2_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_read(path = config_file,
         data_product = data_product1,
         use_version = version1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Run tests
test_that("df is returned", {
  tmp <- read_table(handle = handle,
                    data_product = data_product1,
                    component = component)
  expect_equivalent(as.data.frame(tmp), df)
})

test_that("df2 is returned", {
  tmp <- read_table(handle = handle,
                    data_product = data_product1,
                    component = component2)
  expect_equivalent(as.data.frame(tmp), df2)
})
