library("rhdf5")
library(readxl)

#Create subgroup for processed population data
h5createGroup("scrc_demographics.h5", "processeddata/SIMD_income")

simd<-read.csv("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/DataZone2011_simd2020.csv")
simd_income<-simd[,c(1,34)]
simd_income[,1]<-as.character(simd_income[,1])            
h5write(simd_income, "scrc_demographics.h5",name="processeddata/SIMD_income/simd_datazone_income")
