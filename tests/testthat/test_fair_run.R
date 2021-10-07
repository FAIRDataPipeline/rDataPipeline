context("Testing fair_run()")


# Write data products to registry -----------------------------------------

uid <- random_hash()
data_product1 <- paste("test/fairrun/data", uid, 1, sep = "/")
data_product2 <- paste("test/fairrun/data", uid, 2, sep = "/")
data_product3 <- paste("test/fairrun/data", uid, 3, 1, sep = "/")
coderun_description <- "Register data in the pipeline"
dataproduct_description <- "Data product"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- paste0("config_files/fair_run/config_", uid, ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "txt")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          file_type = "txt")
add_write(path = config_file,
          data_product = data_product3,
          description = dataproduct_description,
          file_type = "txt")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
cat(paste0(uid, 1), file = path1)

path2 <- link_write(handle, data_product2)
cat(paste0(uid, 2), file = path2)

path3 <- link_write(handle, data_product3)
cat(paste0(uid, 3), file = path3)

finalise(handle)

# Read with globbing ------------------------------------------------------

# uid <- random_hash()
# data_product1 <- paste0(paste("test/fairrun/data", uid, sep = "_"), "/**")
# coderun_description <- "Register a file in the pipeline"
# dataproduct_description <- "Data product"
# namespace1 <- "username"
#
# # User written config file
# config_file <- paste0("config_files/multiple/config2_", uid , ".yaml")
# create_config(path = config_file,
#              description = coderun_description,
#              input_namespace = namespace1,
#              output_namespace = namespace1)
# add_read(path = config_file,
#                  data_product = data_product1)
#
# # CLI functions
# fair_run(path = config_file, skip = TRUE)
#
# # Initialise code run
# config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
# script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
# handle <- initialise(config, script, endpoint)
