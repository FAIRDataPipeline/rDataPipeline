context("Test upload_toml_to_github()")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

runtest <- Sys.getenv("RUN_TEST")

sleep_time <- 0.5

test_user <- "22"

version <- paste0("0.", format(Sys.time(), "%d%m%y%H%M%S"), ".0")

UID <- paste0("toml_test_", format(Sys.time(), "%d%m%y%H%M%S"))

local_dir <- paste0("tests/", UID)

toml_file_name <- paste0(version, ".toml")

create_estimate(toml_file_name,
                local_dir,
                list(test_parameter_1 = UID))

path_to_toml <- paste0(local_dir, "/", toml_file_name)

test_that("upload_toml_to_github works as intended", {
  skip_if(runtest == "")
  expect_message(upload_toml_to_github(path_to_toml))
})

file.remove(path_to_toml)

unlink(local_dir, recursive = TRUE)
