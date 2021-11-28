context("Test read_distribution")

coderun_description <- "Testing read_distribution()"
dataproduct_description <- "Estimate of symptom delay"
uid <- random_hash()
data_product <- paste("test/distribution/symptom-delay", uid, sep = "_")
component1 <- "symptom-delay"
component2 <- "symptom-delay2"

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

# Write data
dat1 <- list(distribution = "Gaussian",
             parameters = list(mean = -16.08, SD = 30))

write_distribution(handle = handle,
                   data_product = data_product,
                   component = component1,
                   distribution = dat1$distribution,
                   parameters = dat1$parameters,
                   description = "symptom delay")

dat2 <- list(distribution = "Gaussian",
             parameters = list(mean = 1, SD = 3))

write_distribution(handle = handle,
                   data_product = data_product,
                   component = component2,
                   distribution = dat2$distribution,
                   parameters = dat2$parameters,
                   description = "symptom delay")

# Finalise code run
finalise(handle)

# Start tests -------------------------------------------------------------

# Generate user-written config file
config_file <- tempfile(fileext = ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_read(data_product = data_product)

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")
handle <- initialise(config, script)

tmp <- list(distribution = dat1$distribution,
            SD = dat1$parameters$SD,
            mean = dat1$parameters$mean)

test_that("function works correctly", {
  testthat::expect_true(is.null(handle$inputs))
  dist1 <- read_distribution(handle = handle,
                             data_product = data_product,
                             component = component1)
  testthat::expect_equivalent(dist1, tmp)
  testthat::expect_false(is.null(handle$inputs))
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(handle$inputs$data_product, data_product)
  dist2 <- read_distribution(handle = handle,
                             data_product = data_product,
                             component = component1)
  testthat::expect_equal(nrow(handle$inputs), 1)
  testthat::expect_equal(dist1, dist2)
})
