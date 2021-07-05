context("Testing findme()")

uid <- random_hash()
data_product1 <- paste("findme/test/array", uid, sep = "_")
data_product2 <- paste("findme/test/array2", uid, sep = "_")
component1 <- "component1/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
version1 <- "0.1.0"
version2 <- "0.2.0"
namespace1 <- "username"

# Write v0.1.0 of test/array to local registry and data store ---------------

# User written config file
config_file <- "config_files/findme/config1.yaml"
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

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component1,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Write v0.2.0 of test/array to local registry and data store ---------------

# User written config file
config_file <- "config_files/findme/config2.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product1,
                  description = dataproduct_description,
                  version = version2)

# CLI functions
fair_pull(config_file)
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component1,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product1,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Write v0.1.0 of test/array2 to local registry and data store --------------

# User written config file
config_file <- "config_files/findme/config3.yaml"
write_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace1,
             output_namespace = namespace1)
write_dataproduct(path = config_file,
                  data_product = data_product2,
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
write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product2,
            component = component1,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = data_product2,
                            component = component2,
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)),
                            dimension_values = list(NA, 10),
                            dimension_units = list(NA, "km"),
                            units = "s")

raise_issue(index = component_id,
            handle = handle,
            issue = "some issue",
            severity = 7)

finalise(handle)

# Start tests -------------------------------------------------------------

# file <- unique(handle$outputs$path)
#
# # Find v0.1.0 of test/array2
# findme(file)
#
# # Find all entries with matching hash
# findme(file, filter = FALSE)
#
