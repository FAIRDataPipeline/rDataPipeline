## hdf5processing

Functions to generate and process hd5f files.

To install:
```{r}
library(devtools)
install_github("ScottishCovidResponse/SCRCdataAPI")
```

To generate hdf5 files in R:
```{r}
library(hdf5r)
H5File$new(filename)
```

In the following example, we will populate "array.h5" with array type data downloaded from `https://github.com/ScottishCovidResponse/simple_network_sim/blob/master/sample_output_files/sample-1591198608.csv`. The component path is input via the 
`component` argument, e.g. group `dz`, subgroup `total`. 

If you want to populate a group directly, simply enter the name of your chosen group into the component argument. Currently, only 2 group levels are supported. If you require more, let me know.


```{r}
download.file("https://github.com/ScottishCovidResponse/simple_network_sim/raw/master/sample_output_files/sample-1591198608.csv", "sample.csv")
sample <- read.csv("sample.csv")
```

To add this data to `table.h5`:
```{r}
create_table(filename = "table.h5", component = "dz/total", df = sample)
```

To check contents:
```{r}
file.h5 <- H5File$new(filename)
file.h5$ls(recursive = TRUE)
file.h5[["dz/total/table"]][]
file.h5$close_all()
```


In the following example, we will populate "array.h5" with array type data.

To create an array:
```{r}
filename <- "array.h5"
component <- "dz/total"
array <- matrix(1:10, 5)
colnames(array) <- paste0("age", 1:2)
rownames(array) <- paste0("dz", 1:5)
dimension_names <- list(`area names` = rownames(array), 
`age classes` = colnames(array))

# demonstrating column values with missing row values
dimension_values <- list(NA, data.frame(a = 1:2, b = 3:4))
dimension_units <- list("10km", "5years")

create_array(filename, component, array, dimension_names, dimension_values, dimension_units)
```

To check contents:
```{r}
file.h5 <- H5File$new(filename)
file.h5$ls(recursive = TRUE)
file.h5[["dz/total/Dimension_1_names"]][]
file.h5[["dz/total/Dimension_1_title"]][]
file.h5[["dz/total/Dimension_2_names"]][]
file.h5[["dz/total/Dimension_2_title"]][]
file.h5[["dz/total/Dimension_2_units"]][]
file.h5[["dz/total/Dimension_2_values"]][]
file.h5$close_all()
```



