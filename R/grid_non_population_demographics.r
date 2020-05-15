# 3
library(rgdal)
library(ggplot2)
library(stringr)
library(readxl)
library(sf)
library(tidyverse)
library(spdep)
library(rhdf5)
library(magrittr)

h5filename <- "scrc_demographics.h5"

# Single age class
grid.size <- 10000

# Read in 2018 population estimates for Scotland in 2018 and extract datazone 
# codes and population size
datazone.demographics <- h5read(file = h5filename, 
                                name = file.path("processeddata/SIMD_income", 
                                                 "simd_datazone_income"))

# Read in datazones shapefile and check for non-intersecting geometries
shape <- st_read("data-raw/shapefiles/SG_DataZone_Bdry_2011.shp")
check <- sf::st_is_valid(x, reason = TRUE)
if (any(check != "Valid Geometry")) {
  datazones <- st_make_valid(shape)
  assertthat::assert_that(sum(st_area(shape)) == sum(st_area(datazones)))
} else 
  datazones <- shape

# Make grid of n*n (each measured in metres) over bounding box of datazone 
# shapefile. It should be possible to replace this with any sf object which 
# contains a grid of polygons to end with the same effect.
grids <- st_make_grid(st_as_sfc(st_bbox(datazones)), 
                        cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = seq_along(.))

# Use bounding box grid to "crop" datazones so that parts in different grid 
# cells are distinct
intersected_grid_contain <- st_intersection(grids, datazones) 

# Find area of each of the newly created distinct datazone components
intersection_datazone_grid <- intersected_grid_contain %>% 
  data.frame() %>% 
  tibble::remove_rownames() %>% 
  select(grid_id, DataZone) %>% 
  rename(datazone = DataZone) %>% 
  merge(datazone.demographics, by = "datazone", all.x = T)

grid_demographics <- intersection_datazone_grid %>% 
  group_by(grid_id, .drop = FALSE) %>% 
  summarise(mean = mean(simd2020_inc_rate))

h5write(grid_demographics, file = h5filename,
        name = "griddata/income_deprivation_scotland_10kmgrid")
