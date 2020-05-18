## demographics_aggregation_scrc

### Population data 

geographies_split_by_area folder contains scripts which split populations between grid cells base on the area of the census geography which each grid cell contains. I'm working on updating it to make sure it works however, the postcode based conversion should probably be used as it is likely to be closert to reality.

geographies_split_by_postcode folder contains scripts which split populations between grid cells base on the proportion of the postcodes contained in the census geography which are in each of the grid cells. 

Both folders contain different scripts for splitting  single age and multiple age class population data. 

processing_population_data.r takes raw population from National Records for Scotland and processes this to useable versions. Raw data and processed data are stored in hdf5 file.

### Demographic data

processing_demographic_data.r (currently) takes raw SIMD data and processes this to a useable version for income deprivation. Processed data is stored in hdf5 file.

grid_non_population_demographics.r calculates the mean value of the demographic data of the census geographies contained in each grid cell. This works for income deprivation (I think) which is all I've looked at so far but might have to be changed for other demographic indicators.

### hdf5 file
Im not sure how best to share this, the version in the repository isn't the correct version and wont update but my computer wont let me change the .gitignore to stop uploading it...


```{r}
library(demographicData)
library(rhdf5)


# Generate hdf5 file
demographics()

filename <- "scrc_demographics.h5"

h5ls(filename)

h5readAttributes(file = filename, name = "scotland_2018")
h5readAttributes(file = filename, name = "scotland_2018/datazone")
h5readAttributes(file = filename, name = "scotland_2018/allages_postcode_10k")
h5readAttributes(file = filename, name = "scotland_2018/allages_area_10k")
h5readAttributes(file = filename, name = "scotland_2018/groupages_area_10k")
h5readAttributes(file = filename, name = "scotland_2018/groupages_postcode_10k")


h5read(file = filename, name = "scotland_2018/groupages_postcode_10k")[,-1] %>%
  rowSums() %>% head()
h5read(file = filename, name = "scotland_2018/allages_postcode_10k")[,-1] %>% 
  head()

h5read(file = filename, name = "scotland_2018/groupages_area_10k")[,-1] %>%
  rowSums() %>% head()
h5read(file = filename, name = "scotland_2018/allages_area_10k")[,-1] %>% 
  head()

h5read(file = filename, name = "simd_income/datazone") %>% head()
h5read(file = filename, name = "simd_income/grid_10k") %>% head()


```