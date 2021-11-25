context("Testing multiple coderuns")

# Write to registry -------------------------------------------------------

coderun_description <- "Testing multiple coderuns"
dataproduct_description <- "A csv file"
uid <- as.character(random_hash())
data_product <- file.path("output", "data", uid, "*")

# Generate user-written config file
config_file <- paste0(tempfile(), ".yaml")
create_config(init_yaml = Sys.getenv("INIT_YAML"),
              path = config_file,
              description = coderun_description,
              script = "echo hello") %>%
  add_write(data_product = data_product,
            description = dataproduct_description,
            file_type = "csv")

# Generate working config file
cmd <- paste("fair run", config_file, "--ci")
working_config_dir <- system(cmd, intern = TRUE)

# Initialise code run
config <- file.path(working_config_dir, "config.yaml")
script <- file.path(working_config_dir, "script.sh")

# for (i in 1:3) {
#   handle <- initialise(config, script)
#
#   # Write data
#   data_product_x <- file.path(dirname(data_product), i)
#   path <- link_write(handle, data_product_x)
#   uid_x <- paste0(uid, "_", i)
#   df_x <- data.frame(a = uid_x, b = uid_x)
#   write.csv(df_x, path)
#
#   finalise(handle)
#
#   test_that("data products recorded in working config", {
#     testthat::expect_equal(handle$outputs$data_product, data_product_x)
#     testthat::expect_equal(handle$outputs$use_version, "0.0.1")
#   })
#
#   output_component_url <- get_entity(handle$code_run)$outputs
#   assertthat::assert_that(length(output_component_url) == 1)
#   output_object_url <- get_entity(output_component_url[[1]])$object
#   output_dp_url <- get_entity(output_object_url)$data_product
#   assertthat::assert_that(length(output_dp_url) == 1)
#   output_dp_name <- get_entity(output_dp_url[[1]])$name
#
#   test_that("data products recorded in registry", {
#     testthat::expect_equal(output_dp_name, data_product_x)
#   })
# }
