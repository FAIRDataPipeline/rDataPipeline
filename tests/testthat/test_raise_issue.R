context("Testing findme()")

uid <- as.character(random_hash())
version <- "0.1.0"
namespace <- "username"
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "a test array"
endpoint <- Sys.getenv("FDP_endpoint")

# Test writing issues to component ----------------------------------------

data_product <- paste("findme/test/array", uid, sep = "_")
component <- "component/a/s/d/f/s"

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

component_id <- write_array(array = as.matrix(df),
                            handle = handle,
                            data_product = data_product,
                            component = component,
                            description = "Some description",
                            dimension_names = list(rowvalue = rownames(df),
                                                   colvalue = colnames(df)))

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "some issue"
severity <- 7
raise_issue(index = component_id,
            handle = handle,
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product)
  testthat::expect_equal(handle$issues$use_component, component)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to whole object by reference --------------------------

data_product <- paste("findme/test/array2", uid, sep = "_")
component <- "component/a/s/d/f/s"

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config2_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version,
          file_type = "csv")

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product)

write.csv(df, path)

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "another issue"
severity <- 9
raise_issue(handle = handle,
            data_product = data_product,
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product)
  testthat::expect_equal(handle$issues$use_component, NA)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to whole object by index --------------------------

data_product <- paste("findme/test/array2b", uid, sep = "_")
component <- "component/a/s/d/f/s"

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config2b_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)
component_id1 <- write_array(array = as.matrix(df),
                             handle = handle,
                             data_product = data_product,
                             component = component,
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df),
                                                    colvalue = colnames(df)))

component_id2 <- write_array(array = as.matrix(df),
                             handle = handle,
                             data_product = data_product,
                             component = "component2",
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df),
                                                    colvalue = colnames(df)))

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "another issue"
severity <- 9
raise_issue(index = component_id,
            handle = handle,
            issue = issue,
            severity = severity,
            whole_object = TRUE)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product, data_product)
  testthat::expect_equal(handle$issues$use_component, NA)
  testthat::expect_equal(handle$issues$issue, issue)
  testthat::expect_equal(handle$issues$severity, severity)
})

# Test writing issues to multiple components by index ---------------------

data_product <- paste("findme/test/array3", uid, sep = "_")
component <- "component/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config3_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

component_id1 <- write_array(array = as.matrix(df),
                             handle = handle,
                             data_product = data_product,
                             component = component,
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df),
                                                    colvalue = colnames(df)))

df2 <- data.frame(a = uid, b = uid, c = uid)

component_id2 <- write_array(array = as.matrix(df2),
                             handle = handle,
                             data_product = data_product,
                             component = component2,
                             description = "Some description",
                             dimension_names = list(rowvalue = rownames(df2),
                                                    colvalue = colnames(df2)))

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple components"
severity <- 7
raise_issue(index = c(component_id1, component_id2),
            handle = handle,
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(unique(handle$issues$use_data_product), data_product)
  testthat::expect_equal(handle$issues$use_component[1], component)
  testthat::expect_equal(handle$issues$use_component[2], component2)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

# Test writing issues to multiple components by reference -----------------

data_product <- paste("findme/test/array4", uid, sep = "_")
component <- "component/a/s/d/f/s"
component2 <- "component2/a/s/d/f/s"

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config4_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
df <- data.frame(a = uid, b = uid)

write_array(array = as.matrix(df),
            handle = handle,
            data_product = data_product,
            component = component,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df),
                                   colvalue = colnames(df)))

df2 <- data.frame(a = uid, b = uid, c = uid)

write_array(array = as.matrix(df2),
            handle = handle,
            data_product = data_product,
            component = component2,
            description = "Some description",
            dimension_names = list(rowvalue = rownames(df2),
                                   colvalue = colnames(df2)))

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple components"
severity <- 7

raise_issue(handle = handle,
            data_product = data_product,
            component = c(component, component2),
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(unique(handle$issues$use_data_product), data_product)
  testthat::expect_equal(handle$issues$use_component[1], component)
  testthat::expect_equal(handle$issues$use_component[2], component2)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

# Test writing issues to multiple objects by index by reference -----------

data_product <- paste("findme/test/array5-one", uid, sep = "_")
data_product2 <- paste("findme/test/array5-two", uid, sep = "_")

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config5_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_write(path = config_file,
          data_product = data_product,
          description = dataproduct_description,
          version = version,
          file_type = "csv")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          version = version,
          file_type = "csv")

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
uid <- paste0(uid, 4)
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product)
write.csv(df, path)

uid <- paste0(uid, 5)
df <- data.frame(a = uid, b = uid)
path <- link_write(handle, data_product2)
write.csv(df, path)

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple data products"
severity <- 9
raise_issue(handle = handle,
            data_product = c(data_product, data_product2),
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product[1], data_product)
  testthat::expect_equal(handle$issues$use_data_product[2], data_product2)
  testthat::expect_equal(unique(handle$issues$use_component), NA)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

finalise(handle)

# Test writing issues to multiple objects by index ------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config6_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product,
         version = version)
add_read(path = config_file,
         data_product = data_product2,
         version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Read data (write to handle)
path <- link_read(handle, data_product)
path <- link_read(handle, data_product2)

test_that("handle issues is null", {
  testthat::expect_true(is.null(handle$issues))
})

issue <- "issue with multiple data products"
severity <- 9
raise_issue(handle = handle,
            data_product = c(data_product, data_product2),
            issue = issue,
            severity = severity)

test_that("handle contains issues block", {
  testthat::expect_true(is.data.frame(handle$issues))
  testthat::expect_equal(handle$issues$use_data_product[1], data_product)
  testthat::expect_equal(handle$issues$use_data_product[2], data_product2)
  testthat::expect_equal(unique(handle$issues$use_component), NA)
  testthat::expect_equal(unique(handle$issues$issue), issue)
  testthat::expect_equal(unique(handle$issues$severity), severity)
})

# Test writing issues to config -------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config7_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product,
         version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

config_issue <- "issue with config"
config_severity <- 8
raise_issue_config(handle = handle,
                   issue = config_issue,
                   severity = config_severity)

test_that("config issue is in handle", {
  testthat::expect_equal(handle$issues$type, "config")
  testthat::expect_equal(handle$issues$issue, config_issue)
  testthat::expect_equal(handle$issues$severity, config_severity)
})

finalise(handle)

test_that("config issue is in registry", {
  component_url <- get_entity(handle$model_config)$components
  testthat::expect_equal(length(component_url), 1)

  issue_url <- get_entity(component_url[[1]])$issues
  testthat::expect_equal(length(issue_url), 1)
  tmp <- get_entity(issue_url[[1]])

  testthat::expect_equal(tmp$description, config_issue)
  testthat::expect_equal(tmp$severity, config_severity)
})

# Test writing issues to script -------------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config8_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product,
         version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

script_issue <- "issue with script"
script_severity <- 9
raise_issue_script(handle = handle,
                   issue = script_issue,
                   severity = script_severity)

test_that("script issue is in handle", {
  testthat::expect_equal(handle$issues$type, "script")
  testthat::expect_equal(handle$issues$issue, script_issue)
  testthat::expect_equal(handle$issues$severity, script_severity)
})

finalise(handle)

test_that("script issue is in registry", {
  component_url <- get_entity(handle$submission_script)$components
  testthat::expect_equal(length(component_url), 1)

  issue_url <- get_entity(component_url[[1]])$issues
  testthat::expect_equal(length(issue_url), 1)
  tmp <- get_entity(issue_url[[1]])

  testthat::expect_equal(tmp$description, script_issue)
  testthat::expect_equal(tmp$severity, script_severity)
})

# Test writing issues to GitHub repo --------------------------------------

# User written config file
config_file <- file.path(tempdir(), "config_files", "raise_issue",
                         paste0("config9_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace,
              output_namespace = namespace)
add_read(path = config_file,
         data_product = data_product,
         version = version)

# CLI functions
fair_run(config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

repo_issue <- "issue with repo"
repo_severity <- 2
raise_issue_repo(handle = handle,
                 issue = repo_issue,
                 severity = repo_severity)

test_that("repo issue is in handle", {
  testthat::expect_equal(handle$issues$type, "repo")
  testthat::expect_equal(handle$issues$issue, repo_issue)
  testthat::expect_equal(handle$issues$severity, repo_severity)
})

finalise(handle)

test_that("repo issue is in registry", {
  component_url <- get_entity(handle$code_repo)$components
  testthat::expect_equal(length(component_url), 1)

  issue_url <- get_entity(component_url[[1]])$issues
  testthat::expect_equal(length(issue_url), 1)
  tmp <- get_entity(issue_url[[1]])

  testthat::expect_equal(tmp$description, repo_issue)
  testthat::expect_equal(tmp$severity, repo_severity)
})
