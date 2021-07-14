context("Testing link_read()")

uid <- as.character(random_hash())
data_product1 <- paste("test/multiple/coderuns", uid, sep = "_")
use_data_product <- paste0(data_product, "-${{DPAPI.RUN_ID}}")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/multiple/config_", uid , ".yaml")
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  file_type = "csv",
                  use_data_product = use_data_product)

# CLI functions
fair_pull(path = config_file, endpoint = endpoint)
fair_run(path = config_file, endpoint = endpoint, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script, endpoint)

# Write data
path <- link_write(handle, data_product1)
df <- data.frame(a = uid, b = uid)
write.csv(df, path)

finalise(handle, endpoint)
