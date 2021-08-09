# Examples taken from
# https://fairdatapipeline.github.io/docs/interface/example0/
#

library(rFDP)

uid <- random_hash()
namespace <- "username"
coderun_description <- "Nice description"

# Empty code run ----------------------------------------------------------

# User-written config
config_file <- paste0("config_files/1/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle)


# Write data product (HDF5) ------------------------------------------------

data_product_hdf5 <- "test/array"
component_hdf5 <- "component1/a/s/d/f/s"

# User-written config
config_file <- paste0("config_files/2/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = "test/array",
          description = "test array with simple data")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product_hdf5,
            component = component_hdf5,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)),
            dimension_values = list(NA, 10),
            dimension_units = list(NA, "km"),
            units = "s")

finalise(handle)

# Read data product (HDF5) --------------------------------------------------

# User-written config
config_file <- paste0("config_files/3/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product_hdf5)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

dat <- read_array(handle = handle,
                  data_product = data_product_hdf5,
                  component = component_hdf5)

finalise(handle)

# Write data product (csv) --------------------------------------------------

data_product_csv <- "test/csv"

# User-written config
config_file <- paste0("config_files/4/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product_csv,
          description = "test csv file with simple data",
          file_type = "csv")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

path <- link_write(handle, data_product_csv)

write.csv(df, path)

finalise(handle)

# Read data product (csv) ---------------------------------------------------

# User-written config
config_file <- paste0("config_files/5/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product_csv)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

path <- link_read(handle, data_product_csv)
df <- read.csv(path)

finalise(handle)

# Write data product (point estimate) ---------------------------------------

data_product_est <-"test/estimate/asymptomatic-period"
component_est <- "asymptomatic-period"

# User-written config
config_file <- paste0("config_files/6/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product_est,
          description = "asymptomatic period")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

write_estimate(value = 9,
               handle = handle,
               data_product = data_product_est,
               component = component_est,
               description = "asymptomatic period")

finalise(handle)

# Read data product (point estimate) ----------------------------------------

# User-written config
config_file <- paste0("config_files/7/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product_est)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

read_estimate(handle = handle,
              data_product = data_product_est,
              component = component_est)

finalise(handle)

# Write data product (distribution) -----------------------------------------

data_product_dist <- "test/distribution/symptom-delay"
component_dist <- "symptom-delay"

# User-written config
config_file <- paste0("config_files/8/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product_dist,
          description = "Estimate of symptom delay")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

write_distribution(handle = handle,
                   data_product = data_product_dist,
                   component = component_dist,
                   distribution = "Gaussian",
                   parameters = list(mean = -16.08, SD = 30),
                   description = "symptom delay")

finalise(handle)

# Read data product (distribution) ------------------------------------------

# User-written config
config_file <- paste0("config_files/9/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product_dist)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

read_distribution(handle = handle,
                  data_product = data_product_dist,
                  component = component_dist)

finalise(handle)

# Attach issue to component -------------------------------------------------

data_product_issues <- "test/array/issues"

# User-written config
config_file <- paste0("config_files/10/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product_issues,
          description = "a test array")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

df <- data.frame(a = 1:2, b = 3:4)
rownames(df) <- 1:2

component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = data_product_issues,
                            component = "component1/a/s/d/f/s",
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)),
                            dimension_values = list(NA, 10),
                            dimension_units = list(NA, "km"),
                            units = "s")

issue <- "some issue"
severity <- 7

raise_issue(index = component_id,
            handle = handle,
            issue = issue,
            severity = severity)

finalise(handle)

# Attach issue to whole data product ----------------------------------------

data_product_issues2 <- "test/array/issues2"

# User-written config
config_file <- paste0("config_files/11/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product_issues = data_product_issues2,
          description = "a test array", file_type = "csv")

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle)

# Attach issue to config ----------------------------------------------------

# User-written config
config_file <- paste0("config_files/12/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

config_issue <- "issue with config"
config_severity <- 7

raise_issue_config(handle = handle,
                   issue = config_issue,
                   severity = config_severity)

finalise(handle)

# Attach issue to submission script -----------------------------------------

# User-written config
config_file <- paste0("config_files/13/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

script_issue <- "issue with script"
script_severity <- 7

raise_issue_script(handle = handle,
                   issue = script_issue,
                   severity = script_severity)

finalise(handle)

# Attach issue to GitHub repository -----------------------------------------

# User-written config
config_file <- paste0("config_files/14/config_", uid , ".yaml")
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)

# CLI functions
fair_pull(path = config_file)
fair_run(path = config_file, skip = TRUE)

# Initialise CodeRun
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

repo_issue <- "issue with repo"
repo_severity <- 7

raise_issue_repo(handle = handle,
                 issue = repo_issue,
                 severity = repo_severity)

finalise(handle)

# Attach issue to external object -------------------------------------------

# Attach issue to code run --------------------------------------------------

# Delete DataProduct (optionally) if identical to previous version ----------

# Delete CodeRun (optionally) if nothing happened ---------------------------

# CodeRun with aliases (use block example) ----------------------------------

# CodeRun with read globbing ------------------------------------------------

# CodeRun with write globbing -----------------------------------------------
