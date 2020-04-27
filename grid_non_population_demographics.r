rm(list=ls())
library("rgdal")
library("ggplot2")
library(stringr)
library(readxl)
library(sf)
library(tidyverse)
library(spdep)
library("rhdf5")

##Single age class
grid.size=10000

#Read in 2018 population estimates for Scotland in 2018 and extract datazone codes and population size
datazone.demographics <- h5read(file="scrc_demographics.h5", name="processeddata/SIMD_income/simd_datazone_income")
colnames(datazone.demographics)[1]<-c("datazone")
#Read in datazones shapefile
shape<-readOGR(dsn = "C:/Users/2117658m/Documents/demographics_scrc_shape_files/SG_DataZone_Bdry_2011.shp")
#spacial object to sf object
datazones <- st_as_sf(shape)
#Make grid of n*n (each measured in metres) over bounding box of datazone shapefile. It should be possible to replace this with any sf object which contains a grid of polygons to end with the same effect.
grid_50 <- st_make_grid(st_as_sfc(st_bbox(datazones)), cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = 1:length(.))
#use bounding box grid to "crop" datazones so that parts in different grid cells are distinct
intersected_grid_contain<-st_intersection(grid_50, datazones)
#find area of each of the newly created distinct datazone components
intersection_datazone_grid<-data.frame("grid_id"=intersected_grid_contain$grid_id, "datazone"=as.character(intersected_grid_contain$DataZone))
intersection_datazone_grid<-left_join(intersection_datazone_grid, datazone.demographics, by="datazone")
grid_demographics<-intersection_datazone_grid %>% group_by(grid_id, .drop = FALSE) %>% summarise(mean = mean(simd2020_inc_rate))

