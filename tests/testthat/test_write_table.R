context("Testing write_array()")

uid <- random_hash()
namespace1 <- "username"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "Test table"
data_product1 <- paste("test/table", uid, sep = "_")
component1 <- "a/b/c/d"
version1 <- "0.1.0"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/write_table/config_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version1)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

test_that(".h5 file is generated", {
  ind <- write_table(df = df,
                     handle = handle,
                     data_product = data_product1,
                     component = component1,
                     description = "Some description")

  filename <- handle$outputs %>%
    dplyr::filter(index == index) %>%
    dplyr::select(path) %>%
    unlist() %>% unname()

  testthat::expect_true(is.data.frame(rhdf5::h5ls(filename)))
})
