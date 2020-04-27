rm(list=ls())
library("rgdal")
library("ggplot2")
library(stringr)
library(readxl)
library(sf)
library(tidyverse)
library(spdep)
library("rhdf5")

##Multiple age classes
#Desired structure of ageclasses (lower age bound of each class)
age_class_structure<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)
number_age_classes<-length(age_class_structure)
grid.size<-10000
#First section takes dataset of single year age data and transforms it to the desired format
#Single year age data should be in the format: 1st column - Geographical identifier, columns 2:92 single year population totals for age 0 to 89 and 90+ ageclass

datazone.populations.full <- h5read(file="scrc_demographics.h5", name="processeddata/populations_single_year/datazone_population_persons_singleyear_2013")
age_class_datazone<-matrix(data=0, ncol=number_age_classes, nrow=nrow(datazone.populations.full))

{
for(age.class in 1:(number_age_classes-1)){
  age_class_datazone[,age.class]<-rowSums(datazone.populations.full[,c(((age_class_structure[age.class]+1):(age_class_structure[age.class]+5))+1)])
}
age_class_datazone[,number_age_classes]<-datazone.populations.full[,ncol(datazone.populations.full)]
age_class_datazone<-data.frame("DataZone"=datazone.populations.full[,1], age_class_datazone)
colnames(age_class_datazone)[2:(number_age_classes+1)]<-age_class_structure
}
#Read in datazones shapefile
shape<-readOGR(dsn = "C:/Users/2117658m/Documents/demographics_scrc_shape_files/SG_DataZone_Bdry_2011.shp")
#spacial object to sf object
datazones <- st_as_sf(shape)
#Read in grid shapefile or make grid
grid_50 <- st_make_grid(st_as_sfc(st_bbox(datazones)), cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = 1:length(.))
#use bounding box grid to "crop" datazones so that parts in different grid cells are distinct
intersected_grid_contain<-st_intersection(grid_50, datazones)
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

grid_populations<-matrix(data=0, nrow=nrow(wide_new_table_pop_sizes), ncol=(number_age_classes+1))
grid_populations[,1]<-unlist(wide_new_table[,1])
colnames(grid_populations)<-c("grid_id", age_class_structure)
#Loop over each row and find the proportion of the population of each datazone which is in each grid cell (I'm sure theres a more efficient way t do this but not sure what it is!)
for(age.class in 1:number_age_classes){
for(i in 1:ncol(wide_new_table_pop_sizes)
){
  in_gridcell<-wide_new_table_rownames[which(is.na(wide_new_table_rownames[,i])==FALSE),i]
  in_gridcell_pops<-age_class_datazone[match(colnames(in_gridcell),age_class_datazone$DataZone),(age.class+1)]
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
if(length(which(colSums(wide_new_table_pop_sizes)!=age_class_datazone[,(age.class+1)]))!=0){stop()}
#NA's to 0
wide_new_table_pop_sizes<-wide_new_table_pop_sizes %>% replace(is.na(.), 0)
#Add populations from each datazone together to find total population for each grid cell
grid_pop_sizes<-tibble("grid_id"=unlist(wide_new_table[,1]), "population"=rowSums(wide_new_table_pop_sizes))
grid_populations[,(age.class+1)]<-unlist(grid_pop_sizes[,2])
}

#save to hdf file
h5write(grid_populations, file="scrc_demographics.h5", name="griddata/5year_age_class_scotland_10kmgrid")
h5ls("scrc_demographics.h5")
