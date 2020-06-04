## hdf5processing

Functions to generate and process hd5f files.

To install:
```{r}
library(devtools)
install_github("ScottishCovidResponse/hdf5processing")
```

To generate hdf5 files in R:
```{r}
library(hdf5r)
H5File$new(filename)
```

In the following example, we will populate "array.h5" with array type data. 
The array is to be located within the subgroup `total`, within the group `dz`. 
If you want to populate a group directly, simply enter the name of your chosen
group into the component argument. Currently, only 2 group levels are supported. 
If you require more, let me know.

To create an array:
```{r}
filename <- "array.h5"
component <- "dz/total"
array <- matrix(1:10, 5)
colnames(array) <- paste0("age", 1:2)
rownames(array) <- paste0("dz", 1:5)
dimension_names <- list(`age classes` = colnames(array), 
`area names` = rownames(array))

dimension_values <- list(NA, data.frame(x = 1:2, y = 3:4))
dimension_units <- list(NA, "apples")

create_array(filename, component, array, dimension_names, dimension_values,
dimension_units)
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

To create a table:
```{r}
filename <- "table.h5"
component <- "dz/total"
df <- cbind.data.frame(animals = c("duck", "sheep"), count = c(222, 3))

create_table(filename, component, df)
```

To check contents:
```{r}
file.h5 <- H5File$new(filename)
file.h5$ls(recursive = TRUE)
file.h5[["dz/total/table"]][]
file.h5$close_all()
```



