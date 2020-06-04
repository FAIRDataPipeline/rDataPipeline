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
```

scotland_2018 group attributes
```{r}
h5readAttributes(file = h5filename, name = "scotland_2018")
```

dataset attributes
```{r}
h5readAttributes(file = h5filename, name = "scotland_2018/datazone_raw")
h5readAttributes(file = h5filename, name = "scotland_2018/grid10k_binned")
h5readAttributes(file = h5filename, name = "scotland_2018/grid10k_total")
h5readAttributes(file = h5filename, name = "scotland_2018/grid1k_binned")
h5readAttributes(file = h5filename, name = "scotland_2018/grid1k_total")
```

datazone
```{r}
h5read(file = h5filename, name = "scotland_2018/datazone_raw") %>% head()
h5read(file = h5filename, name = "scotland_2018/datazone_id") %>% head()
```

10k
```{r}
h5read(file = h5filename, name = "scotland_2018/grid10k_total") %>% head()
h5read(file = h5filename, name = "scotland_2018/grid10k_binned") %>% head()
h5read(file = h5filename, name = "scotland_2018/grid10k_id") %>% head()
```

1k
```{r}
h5read(file = h5filename, name = "scotland_2018/grid1k_total") %>% head()
h5read(file = h5filename, name = "scotland_2018/grid1k_binned") %>% head()
h5read(file = h5filename, name = "scotland_2018/grid1k_id") %>% head()
```


