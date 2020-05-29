## demographics_aggregation_scrc

```{r}
library(rhdf5)
library(dplyr)

h5filename <- "demographics.h5"
```
scotland_2018 group
```{r}
h5ls(h5filename)
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


