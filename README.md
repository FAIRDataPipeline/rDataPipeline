## SCRCdataAPI

[![](https://img.shields.io/badge/docs-SCRCdataAPI-blue)](https://scottishcovidresponse.github.io/SCRCdataAPI/)
[![test-build](https://github.com/ScottishCovidResponse/SCRCdataAPI/workflows/build/badge.svg)](https://github.com/ScottishCovidResponse/SCRCdataAPI/actions)
[![codecov](https://codecov.io/gh/ScottishCovidResponse/SCRCdataAPI/branch/master/graph/badge.svg?=1)](https://codecov.io/gh/ScottishCovidResponse/SCRCdataAPI)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/7e49a767b4c34f0db4b33fab95616885)](https://app.codacy.com/gh/ScottishCovidResponse/SCRCdataAPI?utm_source=github.com&utm_medium=referral&utm_content=ScottishCovidResponse/SCRCdataAPI&utm_campaign=Badge_Grade_Dashboard)
[![CodeFactor](https://www.codefactor.io/repository/github/scottishcovidresponse/scrcdataapi/badge)](https://www.codefactor.io/repository/github/scottishcovidresponse/scrcdataapi)
[![License: GPL-3.0](https://img.shields.io/badge/licence-GPL--3-yellow)](https://opensource.org/licenses/GPL-3.0)

Functions to generate and process data files for the SCRC data pipeline.

* [Installation](#installation)

To create hdf5 files:
* [Create array](#create-array)
* [Create table](#create-table)

To create toml files:
* [Create distribution](#create-distribution)
* [Create point-estimate](#create-point-estimate)

## Installation

Note to Linux users: installing devtools may require libcurl4-openssl-dev, libhdf5-dev libudunits2-dev, and libgdal-dev.

```{r}
library(devtools)
install_github("ScottishCovidResponse/SCRCdataAPI")
```

and load it into R:

```{r}
library(SCRCdataAPI)
```

## Create table

In the following example, we will populate `test_table.h5` with table type data downloaded from `https://github.com/ScottishCovidResponse/simple_network_sim/blob/master/sample_output_files/sample-1591198608.csv`.  

```{r}
download.file("https://github.com/ScottishCovidResponse/simple_network_sim/raw/master/sample_output_files/sample-1591198608.csv", "sample.csv")
sample <- read.csv("sample.csv")
```

We want to put this data in a directory called `sample1`:

```{r}
# Create *.h5 file
create_table(filename = "test_table.h5", path = "data-raw", component = "sample1", df = sample)
```

Note that the filename argument can take the name of a file you want to create, 
or an existing `*.h5` file.

To read the data file:

```{r}
read_table(filename = "test_table.h5", path = "data-raw", component = "sample1")
```

## Create array

In the following example, we will populate "array.h5" with array type data.

First we generate an array:

```{r}
array <- matrix(1:10, 5)
colnames(array) <- paste0("age", 1:2)
rownames(array) <- paste0("dz", 1:5)
```

Then we extract row and column names (and specify descriptors):

```{r}
dimension_names <- list(`area names` = rownames(array), 
`age classes` = colnames(array))
```

We want to put this data in a directory called `dz`, in a subdirectory called `total`:

```{r}
# Create *.h5 file
create_array(filename = "test_array.h5", path = "data-raw", component = "dz/total", array = array, dimension_names = dimension_names)
```

To read the data file:

```{r}
read_array(filename = "test_array.h5", path = "data-raw", component = "dz/total")
```

## Create distribution

In the following example, we populate "test_distribution.toml":

```{r}
# Create *.toml file
create_distribution(filename = "test_distribution.toml", path = "data-raw", name = "latency", distribution = "gamma", parameters = list(shape = 2.0, scale = 3.0))
```

## Create point-estimate

In the following example, we populate "test_number.toml" with a single point-estimate:

```{r}
# Create *.toml file
create_estimate(filename = "test_number.toml", path = "data-raw", parameters = list(asymptomatic_period = 192.0))
```

To include multiple point-estimates:

```{r}
# Create *.toml file
create_estimate(filename = "test_number.toml", path = "data-raw", parameters = list(asymptomatic_period = 192.0, latent_period = 123.12))
```
