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
datazone.populations.full <- h5read(file="scrc_demographics.h5", name="processeddata/populations_single_year/datazone_population_persons_singleyear_2013")
datazone.populations<-data.frame(datazone.populations[,1],rowSums(datazone.populations.full[,-1]))
colnames(datazone.populations)[1]<-c("DataZone")
#Read in datazones shapefile
shape<-readOGR(dsn = "C:/Users/2117658m/Documents/demographics_scrc_shape_files/SG_DataZone_Bdry_2011.shp")
#spacial object to sf object
datazones <- st_as_sf(shape)
#Make grid of n*n (each measured in metres) over bounding box of datazone shapefile. It should be possible to replace this with any sf object which contains a grid of polygons to end with the same effect.
grid_50 <- st_make_grid(st_as_sfc(st_bbox(datazones)), cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = 1:length(.))
#use bounding box grid to "crop" datazones so that parts in different grid cells are distinct
intersected_grid_contain<-st_intersection(grid_50, datazones)
grid_subset<-grid_50[match(intersected_grid_contain$grid_id,grid_50$grid_id,),]
st_write(grid_subset, dsn="1km_scotland_grid.shp")
#find area of each of the newly created distinct datazone components
intersection_area<-data.frame("grid_id"=intersected_grid_contain$grid_id, "datazone"=intersected_grid_contain$DataZone, "area"=st_area(intersected_grid_contain))
datazone_area<-data.frame("datazone"=datazones$DataZone, "full_zone_area"=st_area(datazones))
#join full area of each datazone to this data.frame and use to find the proportion of each datazone in each grid cell
combined_areas<-left_join(intersection_area, datazone_area, by="datazone")
combined_areas$proportion<-as.numeric(combined_areas$area/combined_areas$full_zone_area)
combined_areas<-combined_areas[,c(1,2,5)]
#Create matrix of grid cells by datazones containing the proportion of each datazone in each grid cell with 0's
wide_new_table<-pivot_wider(combined_areas, names_from = "datazone", values_from = "proportion")
wide_new_table<-wide_new_table %>% replace(is.na(.), 0)

#make new tables to fill in population proportions
wide_new_table_rownames<-wide_new_table[,-1]
wide_new_table_pop_sizes<-wide_new_table[,-1]
rownames(wide_new_table_rownames)<-unlist(wide_new_table[,1])


#Loop over each row and find the proportion of the population of each datazone which is in each grid cell (I'm sure theres a more efficient way t do this but not sure what it is!)
for(i in 1:ncol(wide_new_table_pop_sizes)
){
  in_gridcell<-wide_new_table_rownames[which(is.na(wide_new_table_rownames[,i])==FALSE),i]
  in_gridcell_pops<-datazone.populations[match(colnames(in_gridcell),datazone.populations$DataZone),2]
  rounded_pops<-round(in_gridcell*in_gridcell_pops)#Need better way to do this that doesnt lose people!
  ##WORK AROUND FOR ROUNDING ISSUE - I'm sure theres a better solution to this but I cant think of how to do it!
  #If the rounded population is less than the true population
  #Calculate which areas are the closest to the next integer
  #Add 1 individual to each of the closest areas until the total is met
  #E.g. datazone split into 5 grid cells, population=6, proportion of DZ in each cell: 0.4,0.3,0.2,0.075 and 0.025
  #multiplied by 6:2.40, 1.80, 1.20, 0.45, 0.15. Rounded this would lead to 5/6 individuals being alocated to grid cells: 2,2,1,0,0
  #Minus the allocated population:  0.40, -0.20,  0.20,  0.45,  0.15 .  
  #The largest of these is 0.45 so 1 is added to the corresponding grid square. leading to a final population of 2,1,1,1,0 which contains all of the population in the datazone If 2 "people" were "missing" 1 would be added to the 2 largest numbers, i.e. 0.4 and 0.45
  #
  #Also if the rounding causes a higher population than expected this does the opposite, removing individuals from the grid cells furthest from the nearest integer.
  if(sum(rounded_pops)!=sum(in_gridcell_pops)){
    non_rounded_pops<-(in_gridcell*in_gridcell_pops)
    difference<-non_rounded_pops-rounded_pops
    remainder<-sum(non_rounded_pops)-sum(rounded_pops)
    if(remainder>0){
    next.biggest<-order(difference[,1], decreasing=TRUE)[1:remainder]
    rounded_pops[next.biggest,1]<-rounded_pops[next.biggest,1]+1
    }
    if(remainder<0){
      next.biggest<-order((0-difference[,1]), decreasing=TRUE)[1:(0-remainder)]
      rounded_pops[next.biggest,1]<-rounded_pops[next.biggest,1]-1
    }
  }
  wide_new_table_pop_sizes[which(is.na(wide_new_table_rownames[,i])==FALSE),i]<-rounded_pops
}
#Check that redistributed population size matches original population size
which(colSums(wide_new_table_pop_sizes)!=datazone.populations[,2])
#NA's to 0
wide_new_table_pop_sizes<-wide_new_table_pop_sizes %>% replace(is.na(.), 0)
#Add populations from each datazone together to find total population for each grid cell
grid_pop_sizes<-tibble("grid_id"=unlist(wide_new_table[,1]), "population"=rowSums(wide_new_table_pop_sizes))


#save to hdf file
h5write(grid_pop_sizes, file="scrc_demographics.h5", name="griddata/total_population_1kmgrid")


