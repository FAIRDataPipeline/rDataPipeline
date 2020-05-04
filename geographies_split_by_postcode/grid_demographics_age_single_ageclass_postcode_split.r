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
#'
#' This script transforms age class population data from census geographies to a grid based system. 
#' If the census geography is split across two grid cells this script splits the population between these cells according to the proportion of the postcodes in the geography which are present in each cell.
#' i.e. if a datazone containing 5 postcodes is split between 2 grid cells with 3 postcodes in cell A and 2 in cell B, 60% of the population goes to cell A.
#' It takes:
#'  - shapefile of the census geographies from which the populations should be drawn
#'  - a population dataset containing single-year age classes for each census geography (alternatively a population dataset with preset age classes can be used) in the structure: 1st column - Geographical identifier, columns 2:92 single year population totals for age 0 to 89 and 90+ ageclass
#'  - A grid shapefile which overlays the census geographies. (IF this is not available this script automatically makes a grid, the dimensions of which can be chosen (in m))
#'  - A shapefile containing the boundaries of postcodes contained in the census geographies
#'
#'
#' If a single-year age class dataset is supplied and a different structure is desired (i.e. 5-year age classes) the structure of this new age class must be set as a vector called: age_class_structure
#' This vector should contain the lower age bound of each age class
#' 
#' There is an issue of rounding when the population are being divided between cells, I have a work-around which is explained in the code.
#' 
#' 


grid.size<-10000

#' Age class agreggation
#' 


#First section takes dataset of single year age data and transforms it to the desired format
#Single year age data should be in the format: 1st column - Geographical identifier, columns 2:92 single year population totals for age 0 to 89 and 90+ ageclass
datazone.populations.full <- h5read(file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2018")
empty.datazones<-datazone.populations.full[which(rowSums(datazone.populations.full[,-1])==0),1]
datazone.populations.full<-datazone.populations.full[-match(empty.datazones,datazone.populations.full$DataZone2011Code),]
datazone.populations<-data.frame(datazone.populations.full[,1],rowSums(datazone.populations.full[,-1]))
colnames(datazone.populations)[1]<-c("DataZone")
#' Geography-Grid conversion
#' 

#This section finds what proprotion of each census geography should be in each grid cell

#Read in datazones shapefile
shape<-readOGR(dsn = "C:/Users/2117658m/Documents/demographics_scrc_shape_files/SG_DataZone_Bdry_2011.shp")
datazones <- st_as_sf(shape)

#Read in grid shapefile or make grid. This line should be replaced if a previously set grid is being used. This grid should be imported and then transformed into an sf object
grid_50 <- st_make_grid(st_as_sfc(st_bbox(datazones)), cellsize = c(grid.size, grid.size)) %>% 
  st_sf(grid_id = 1:length(.))
#use bounding box grid to "crop" datazones so that parts in different grid cells are distinct
intersected_grid_contain<-st_intersection(grid_50, datazones)
#Import postcode shapefile
postcode_shp<-readOGR(dsn = "C:/Users/2117658m/Documents/demographics_scrc_shape_files/PC_Cut_20_1.shp")
postcode <- st_as_sf(postcode_shp)
#This postcode doesnt seem to sit within any of the datazone shapefiles, removing it in the absence of a better solution at the moment.
postcode<-postcode[-which(postcode$Postcode=="PA75 6NUB"),]
# Extract which datazone each postcode lies in and then calculate the number of postcodes in each datazone
datazones_contain_postcode<-postcode
st_geometry(datazones_contain_postcode)<-NULL
datazones_contain_postcode<-data.frame(datazones_contain_postcode[,c(1,2,8)])
datazones_contain_postcode_table <- datazones_contain_postcode %>% group_by(DZ11, .drop = FALSE) %>% summarise(nrow = n())
colnames(datazones_contain_postcode_table)[2]<-"postcodes_in_full_DZ"
#Use grid datazone component sf object to find which postcodes are in which components and then calculate the number of postcodes in each component
datazones_grid_contain_postcode<-(st_join(postcode, intersected_grid_contain, join = st_intersects))
st_geometry(datazones_grid_contain_postcode)<-NULL
datazones_grid_contain_postcode<-datazones_grid_contain_postcode[,c(1,2,8,12)]
datazones_grid_contain_postcode<-unique(datazones_grid_contain_postcode[,c(2,3,4)])
datazones_grid_contain_postcode_table <- datazones_grid_contain_postcode %>% group_by(DZ11,grid_id, .drop = FALSE) %>% summarise(nrow = n())
colnames(datazones_grid_contain_postcode_table)[3]<-"postcodes_in_DZ_component"
#Join total postcodes in each datazone to table containing the number of postcodes in each datazone component
postcode_in_and_out<-left_join(datazones_grid_contain_postcode_table, datazones_contain_postcode_table, by="DZ11")
#Calculate the proportion of postcodes in each component
postcode_in_and_out$proportion<-(postcode_in_and_out$postcodes_in_DZ_component/postcode_in_and_out$postcodes_in_full_DZ)
combined_areas<-postcode_in_and_out[,c(1,2,5)]
#Find the total proportion and then correct for where this is over 1 (this is due to postcodes being split by the grid, the correction finds the proportion of the total in each cell)
combined_areas_total_prop<-combined_areas %>% group_by(DZ11, .drop = FALSE) %>% summarise(sum=sum(proportion))
combined_areas<-left_join(combined_areas, combined_areas_total_prop, by="DZ11")
combined_areas$proportion2<-combined_areas$proportion/combined_areas$sum
combined_areas<-combined_areas[,c(1,2,5)]

#Create matrix of grid cells by datazones containing the proportion of postcodes in each datazone in each grid cell with 0's
wide_new_table<-pivot_wider(combined_areas, names_from = "DZ11", values_from = "proportion2", values_fill = list("proportion2"=0))


#remove empty datazones
wide_new_table<-wide_new_table[,-na.omit(match(empty.datazones,colnames(wide_new_table)))]
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
#Add populations from each datazone together to find total population for each grid cell
grid_pop_sizes<-tibble("grid_id"=unlist(wide_new_table[,1]), "population"=rowSums(wide_new_table_pop_sizes))

h5write(grid_populations, file="scrc_demographics.h5", name="griddata/single_age_class_scotland_10kmgrid_postcode")
h5ls("scrc_demographics.h5")
