context("Testing findme()")

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

# Write v0.1.0 of test/array to local registry and data store ---------------

config_file <- "config_files/findme/config1.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

write_array(array = as.matrix(df),
            handle = handle,
            data_product = "findme/test/array",
            component = "component1/a/s/d/f/s",
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = "findme/test/array",
            component = "component2/a/s/d/f/s",
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

# finalise(handle, skip_code_run = TRUE)
finalise(handle)

# Write v0.2.0 of test/array to local registry and data store ---------------

config_file <- "config_files/findme/config2.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

write_array(array = as.matrix(df),
            handle = handle,
            data_product = "findme/test/array",
            component = "component1/a/s/d/f/s",
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

write_array(array = as.matrix(df),
            handle = handle,
            data_product = "findme/test/array",
            component = "component2/a/s/d/f/s",
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

# finalise(handle, skip_code_run = TRUE)
finalise(handle)

# Write v0.1.0 of test/array2 to local registry and data store --------------

config_file <- "config_files/findme/config3.yaml"
fdp_pull(config_file)
fdp_run(config_file, skip = TRUE)

handle <- initialise()

write_array(array = as.matrix(df),
            handle = handle,
            data_product = "findme/test/array2",
            component = "component1/a/s/d/f/s",
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = "findme/test/array2",
                            component = "component2/a/s/d/f/s",
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)),
                            dimension_values = list(NA, 10),
                            dimension_units = list(NA, "km"),
                            units = "s")

issue_with_component(index = component_id,
                     handle = handle,
                     issue = "some issue",
                     severity = 7)

# finalise(handle, skip_code_run = TRUE)
finalise(handle)

# Start tests -------------------------------------------------------------

file <- unique(handle$outputs$path)

# Find v0.1.0 of test/array2
findme(file)

# Find all entries with matching hash
findme(file, filter = FALSE)


