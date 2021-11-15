context("Testing initialise()")

uid <- as.character(random_hash())
data_product1 <- file.path("real", "data", uid, "1")
dataproduct_description <- "a nice description"
coderun_description <- "Do nothing"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# delete_if_empty ---------------------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "initialise",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle, delete_if_empty = TRUE)
