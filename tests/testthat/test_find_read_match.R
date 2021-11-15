context("Testing find_read_match()")

# Write to registry -------------------------------------------------------

uid <- as.character(random_hash())
data_product1 <- file.path("data_product", "read", "wildcard", uid, "1")
data_product2 <- file.path("data_product", "read", "wildcard", uid, "1", "2")
coderun_description <- "Register a file in the pipeline"
dataproduct_description <- "A csv file"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# User written config file
config_file <- file.path(tempdir(), "config_files", "inputglobbing",
                         paste0("config_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_write(path = config_file,
          data_product = data_product1,
          description = dataproduct_description,
          file_type = "csv")
add_write(path = config_file,
          data_product = data_product2,
          description = dataproduct_description,
          file_type = "csv")

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
uid1 <- paste0(uid, "_1")
df1 <- data.frame(a = uid1, b = uid1)
write.csv(df1, path1)

path2 <- link_write(handle, data_product2)
uid2 <- paste0(uid, "_2")
df2 <- data.frame(a = uid2, b = uid2)
write.csv(df2, path2)

finalise(handle)

# Input globbing ----------------------------------------------------------

data_product3 <- file.path("data_product", "read", "wildcard", uid, "*")

# User written config file
config_file <- file.path(tempdir(), "config_files", "inputglobbing",
                         paste0("config2_", uid, ".yaml"))
create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)
add_read(path = config_file,
         data_product = data_product3)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

test_that("data products recorded in working config", {
  reads <- handle$yaml$read
  testthat::expect_equal(reads[[1]]$data_product, data_product2)
  testthat::expect_equal(reads[[2]]$data_product, data_product1)

  testthat::expect_equal(reads[[1]]$use$version, "0.0.1")
  testthat::expect_equal(reads[[2]]$use$version, "0.0.1")

  aliases <- find_read_match(handle, data_product3)
  testthat::expect_true(all(aliases %in% c(data_product1, data_product2)))
  path <- link_read(handle, aliases[1])
  path <- link_read(handle, aliases[2])
})
