# 2

h5filename <- "scrc_demographics.h5"


# Create subgroup for processed population data
h5createGroup(h5filename, "processeddata/SIMD_income")

simd <- read.csv("data-raw/DataZone2011_simd2020.csv")
simd_income <- simd %>% 
  select(dz2011, simd2020_inc_rate) %>% 
  rename(datazone = dz2011)

h5write(simd_income, h5filename,
        name = "processeddata/SIMD_income/simd_datazone_income")
