## demographics_aggregation_scrc


View file structure
```{r}
h5ls("scrc_demographics.h5")
```
scrc_demographics.h5 contains 2 groups, scotland_2018 (containing 5 datasets) 
and simd_income (containing 2 datasets).

View attributes associated with the **scotland_2018 group**
```{r}
h5readAttributes(file = filename, name = "scotland_2018")
```

View attributes associated with each of the 5 datasets in scotland_2018
```{r}
h5readAttributes(file = filename, name = "scotland_2018/datazone")
h5readAttributes(file = filename, name = "scotland_2018/allages_postcode_10k")
h5readAttributes(file = filename, name = "scotland_2018/allages_area_10k")
h5readAttributes(file = filename, name = "scotland_2018/groupages_area_10k")
h5readAttributes(file = filename, name = "scotland_2018/groupages_postcode_10k")
```

View the first 5 lines of each dataset in scotland_2018
```{r}
h5read(file = filename, name = "scotland_2018/datazone") %>% head()

h5read(file = filename, name = "scotland_2018/allages_postcode_10k") %>% head()
h5read(file = filename, name = "scotland_2018/groupages_postcode_10k") %>% head()

h5read(file = filename, name = "scotland_2018/allages_area_10k") %>% head()
h5read(file = filename, name = "scotland_2018/groupages_area_10k") %>% head()
```

View attributes associated with the **simd_income group**
```{r}
h5readAttributes(file = filename, name = "simd_income")
```

View attributes associated with each of the 2 datasets in simd_income
```{r}
h5readAttributes(file = filename, name = "simd_income/datazone")
h5readAttributes(file = filename, name = "simd_income/grid_10k") 
```

View the first 5 lines of each dataset in simd_income
```{r}
h5read(file = filename, name = "simd_income/datazone") %>% head()
h5read(file = filename, name = "simd_income/grid_10k") %>% head()
```