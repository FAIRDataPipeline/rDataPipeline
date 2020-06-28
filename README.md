## SCRCdataAPI

Functions to generate and process data files for the SCRC data pipeline.

* [Installation](#installation)
* [Create array](#create-array)
* [Create table](#create-table)


## Installation

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
create_table(h5filename = "test_table.h5", component = "sample1", df = sample)
```

Note that the filename argument can take the name of a file you want to create, 
or an existing `*.h5` file.

To read the data file:

```{r}
read_table(h5filename = "test_table.h5", path = "sample1")
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
create_array(h5filename = "test_array.h5", component = "dz/total", array, dimension_names)
```

To read the data file:

```{r}
read_array(h5filename = "test_array.h5", path = "dz/total")
```
