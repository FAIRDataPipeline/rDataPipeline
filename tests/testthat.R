library(testthat)
library(rDataPipeline)
library(here())

# Activate virtual environment (for fair CLI)
reticulate::conda_create("r-reticulate", python_version = 3.8)
Sys.setenv(RETICULATE_PYTHON = file.path("~", "anaconda3", "envs",
                                         "r-reticulate", "bin", "python"))
reticulate::conda_install(envname = "r-reticulate", packages = "fair-cli",
                          pip = TRUE)
reticulate::use_condaenv("r-reticulate")
fair <- reticulate::import("fair")

# If this repository has not yet been initialised by fair CLI, run `init --ci`
if (!file.exists(file.path(here(), ".fair")))
  system("fair init --ci")

# Export init.yaml (change this to `.fair/cli-config.yaml` after CLI updates)
init_yaml <- paste0(tempfile(), ".yaml")
system(paste("fair init --export", init_yaml))
Sys.setenv(INIT_YAML = init_yaml)

# Sys.setenv(`_RUN_TESTS_` = "true")

# Don't run tests on CRAN's isolated test rig, since they depend on a local
# registry and CLI being installed and will therefore fail.

if (Sys.getenv("_RUN_TESTS_") != "") {
  Sys.setenv(FDP_endpoint = "http://localhost:8000/api/")
  endpoint <- Sys.getenv("FDP_endpoint")
  testthat::test_check("rDataPipeline")
}
