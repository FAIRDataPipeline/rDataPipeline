rm(list=ls())
library("rgdal")
library(sf)
library(tidyverse)
library(spdep)
library("rhdf5")

#In metres
grid.size=1000
shape<-readOGR(dsn = "C:/Users/2117658m/Documents/aggregating_demographics/shape_files/SG_DataZone_Bdry_2011.shp")
#spacial object to sf object
datazones <- st_as_sf(shape)
#Make grid of n*n (each measured in metres) over bounding box of datazone shapefile. It should be possible to replace this with any sf object which contains a grid of polygons to end with the same effect.
grid_50 <- st_make_grid(st_as_sfc(st_bbox(datazones)), cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = 1:length(.))

#use bounding box grid to "crop" datazones so that parts in different grid cells are distinct
intersected_grid_contain<-st_intersection(grid_50, datazones)
grid_subset<-grid_50[match(intersected_grid_contain$grid_id,grid_50$grid_id,),]
st_write(grid_subset, dsn="1km_scotland_grid.shp")
