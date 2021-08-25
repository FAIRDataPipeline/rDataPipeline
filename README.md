## rDataPipeline

[![](https://img.shields.io/badge/docs-rDataPipeline-blue)](https://FAIRDataPipeline.github.io/rDataPipeline/)
[![test-build](https://github.com/FAIRDataPipeline/rDataPipeline/workflows/build/badge.svg?=1)](https://github.com/FAIRDataPipeline/rDataPipeline/actions)
[![codecov](https://codecov.io/gh/FAIRDataPipeline/rDataPipeline/branch/main/graph/badge.svg?=1)](https://codecov.io/gh/FAIRDataPipeline/rDataPipeline)
[![License: GPL-3.0](https://img.shields.io/badge/licence-GPL--3-yellow)](https://opensource.org/licenses/GPL-3.0)

Functions to generate and process data files for the FAIR data pipeline.

## Installation

In R:

```{r}
library(devtools)
install_github("FAIRDataPipeline/rDataPipeline")
library(rDataPipeline)
```

## User-written *config.yaml*

To generate a Code Run, you need to write a *config.yaml* file. This can be done
manually, or automated in the following way:

```{r}
config_file <- "config.yaml"
coderun_description <- "Register a file in the pipeline"
namespace <- "soniamitchell"

# Generate config.yaml file
create_config(path = config_file,
             description = coderun_description,
             input_namespace = namespace,
             output_namespace = namespace)
             
# Add data product to `write` block
add_write(path = config_file,
          data_product = file.path("real", "data", "1"),
          description = "My first dataset",
          file_type = "csv")
                  
add_write(path = config_file,
          data_product = file.path("real", "data", "2"),
          description = "My second dataset",
          file_type = "csv")
```

Similarly, `add_read()` can be used to add data products to the `read`
block.

## FAIR DataPipeline CLI

After having written a *config.yaml* file, Python-based CLI tools should be
used to generate a code run. As these tools are not yet available, temporary 
functions have been made available in R. 

The R implementation of `fair init()` is used to register a new user in the 
local data registry. This only needs to be done once, when the local registry is
first installed.

```{r}
fair_init(family_name = "Mitchell",
          given_name = "Sonia",
          orcid = "https://orcid.org/0000-0003-1536-2066",
          organisation = c("University of Glasgow",
                           "Boydorr Centre for Population and Ecosystem Health"),
          endpoint = "http://localhost:8000/api/")
```

The R implementation of `fair_pull()` is used to register external data products 
in the local registry and data store. 

Note that the full implementation will also pull data from the remote registry 
and data store -- this has not been implemented in R.

```{r}
fair_pull(path = config_file)
```

The R implementation of `fair_run()` will generate a working config file from 
the user-written *config.yaml* file, containing information required by the 
DataPipeline API. This file is saved in the local data store and the directory
in which it resides is written to a global variable, `FDP_CONFIG_DIR`.

Note that the full implementation will also run the submission script (detailed 
below) -- this has not been implemented in R.

```{r}
fair_run(path = config_file)
```

## Submission script

It is intended that `fair_run()` should run generate a Code Run from a 
submission script. Until the Python-based CLI tools are written, the submission
script can be run interactively in R:

```{r}
# Retrieve location of config file and submission script
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")

# Initialise code run
handle <- initialise(config, script)

# Write data
path1 <- link_write(handle, data_product1)
uid1 <- paste0(uid, "_1")
df1 <- data.frame(a = uid1, b = uid1)
write.csv(df1, path1)

# Write more data
path2 <- link_write(handle, data_product2)
uid2 <- paste0(uid, "_2")
df2 <- data.frame(a = uid2, b = uid2)
write.csv(df2, path2)

# Finalise code run
finalise(handle)
```

## More information

For more information, please visit the FAIR Data Pipeline [website](https://fairdatapipeline.github.io)
