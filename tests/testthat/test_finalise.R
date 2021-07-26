context("Testing finalise()")

uid <- as.character(random_hash())
coderun_description <- "Do nothing"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/finalise/config_", uid , ".yaml")

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle, delete_if_empty = TRUE)
