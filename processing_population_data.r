
library("rhdf5")
library(readxl)
#Create file
h5createFile("scrc_demographics.h5")

#Create group within file for raw data
h5createGroup("scrc_demographics.h5", "rawdata")
#Create subgroup for raw population data
h5createGroup("scrc_demographics.h5", "rawdata/scotlandpopulations")
#Create subgroup for SIMD deprivation data
h5createGroup("scrc_demographics.h5", "rawdata/SIMD")
#Create group within file for processed data
h5createGroup("scrc_demographics.h5", "processeddata")
#Create subgroup for processed population data
h5createGroup("scrc_demographics.h5", "processeddata/scotlandpopulations_single_year")
#Create group within file for grid data
h5createGroup("scrc_demographics.h5", "griddata")

h5ls("scrc_demographics.h5")
#2013
sape_2013_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2013-persons.xlsx",col_names = FALSE)
h5write(sape_2013_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2013")
headings<-as.vector(cbind(sape_2013_persons[6,c(1,2,3)], "all_ages", sape_2013_persons[4,c(6:96)]))
sape_2013_persons <- sape_2013_persons[-c(1:6),]
sape_2013_persons <- sape_2013_persons[,-5]
sape_2013_persons <- sape_2013_persons[-c(nrow(sape_2013_persons)-1,nrow(sape_2013_persons)),]
colnames(sape_2013_persons) <- headings
colnames(sape_2013_persons)[4] <- "all_ages"
sape_2013_persons[,c(4:95)] <- apply(sape_2013_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2013_persons<-sape_2013_persons[,-c(2:4)]
h5write(sape_2013_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2013")

#2014
sape_2014_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2014-persons.xlsx",col_names = FALSE)
h5write(sape_2014_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2014")
headings<-as.vector(cbind(sape_2014_persons[6,c(1,2,3)], "all_ages", sape_2014_persons[4,c(6:96)]))
sape_2014_persons <- sape_2014_persons[-c(1:6),]
sape_2014_persons <- sape_2014_persons[,-5]
sape_2014_persons <- sape_2014_persons[-c(nrow(sape_2014_persons)-1,nrow(sape_2014_persons)),]
colnames(sape_2014_persons) <- headings
colnames(sape_2014_persons)[4] <- "all_ages"
sape_2014_persons[,c(4:95)] <- apply(sape_2014_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2014_persons<-sape_2014_persons[,-c(2:4)]
h5write(sape_2014_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2014")

#2015
sape_2015_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2015-persons.xlsx",col_names = FALSE)
h5write(sape_2015_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2015")
headings<-as.vector(cbind(sape_2015_persons[6,c(1,2,3)], "all_ages", sape_2015_persons[4,c(6:96)]))
sape_2015_persons <- sape_2015_persons[-c(1:6),]
sape_2015_persons <- sape_2015_persons[,-5]
sape_2015_persons <- sape_2015_persons[-c(nrow(sape_2015_persons)-1,nrow(sape_2015_persons)),]
colnames(sape_2015_persons) <- headings
colnames(sape_2015_persons)[4] <- "all_ages"
sape_2015_persons[,c(4:95)] <- apply(sape_2015_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2015_persons<-sape_2015_persons[,-c(2:4)]
h5write(sape_2015_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2015")

#2016
sape_2016_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2016-persons.xlsx",col_names = FALSE)
h5write(sape_2016_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2016")
headings<-as.vector(cbind(sape_2016_persons[6,c(1,2,3)], "all_ages", sape_2016_persons[4,c(6:96)]))
sape_2016_persons <- sape_2016_persons[-c(1:6),]
sape_2016_persons <- sape_2016_persons[,-5]
sape_2016_persons <- sape_2016_persons[-c(nrow(sape_2016_persons)-1,nrow(sape_2016_persons)),]
colnames(sape_2016_persons) <- headings
colnames(sape_2016_persons)[4] <- "all_ages"
sape_2016_persons[,c(4:95)] <- apply(sape_2016_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2016_persons<-sape_2016_persons[,-c(2:4)]
h5write(sape_2016_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2016")

#2017
sape_2017_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2017-persons.xlsx",col_names = FALSE)
h5write(sape_2017_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2017")
headings<-as.vector(cbind(sape_2017_persons[6,c(1,2,3)], "all_ages", sape_2017_persons[4,c(6:96)]))
sape_2017_persons <- sape_2017_persons[-c(1:6),]
sape_2017_persons <- sape_2017_persons[,-5]
sape_2017_persons <- sape_2017_persons[-c(nrow(sape_2017_persons)-1,nrow(sape_2017_persons)),]
colnames(sape_2017_persons) <- headings
colnames(sape_2017_persons)[4] <- "all_ages"
sape_2017_persons[,c(4:95)] <- apply(sape_2017_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2017_persons<-sape_2017_persons[,-c(2:4)]
h5write(sape_2017_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2017")

#2018
sape_2018_persons<- read_excel("C:/Users/2117658m/Documents/aggregating_demographics/raw_data/population_datazone/sape-2018-persons.xlsx",col_names = FALSE)
h5write(sape_2018_persons, file="scrc_demographics.h5", name="rawdata/scotlandpopulations/datazone_population_persons_singleyear_2018")
headings<-as.vector(cbind(sape_2018_persons[6,c(1,2,3)], "all_ages", sape_2018_persons[4,c(6:96)]))
sape_2018_persons <- sape_2018_persons[-c(1:6),]
sape_2018_persons <- sape_2018_persons[,-5]
sape_2018_persons <- sape_2018_persons[-c(nrow(sape_2018_persons)-1,nrow(sape_2018_persons)),]
colnames(sape_2018_persons) <- headings
colnames(sape_2018_persons)[4] <- "all_ages"
sape_2018_persons[,c(4:95)] <- apply(sape_2018_persons[,c(4:95)],2, function(x) as.numeric(x))
sape_2018_persons<-sape_2018_persons[,-c(2:4)]
h5write(sape_2018_persons, file="scrc_demographics.h5", name="processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2018")

#Writing metadata/attributes
fid<-H5Fopen("scrc_demographics.h5")
#Raw data
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2013")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2013 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2014")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2014 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2015")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2015 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2016")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2016 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2017")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2017 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)
did<-H5Dopen(fid, name="/rawdata/scotlandpopulations/datazone_population_persons_singleyear_2018")
h5writeAttribute(did, name="Description", attr="Population of datazones in Scotland in 2018 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series . Downloaded 24/4/20. ")
H5Dclose(did)

#Processed data
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2013")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2013 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2013")
H5Dclose(did)
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2014")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2014 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2014")
H5Dclose(did)
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2015")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2015 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2015")
H5Dclose(did)
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2016")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2016 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2016")
H5Dclose(did)
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2017")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2017 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2017")
H5Dclose(did)
did<-H5Dopen(fid, name="/processeddata/scotlandpopulations_single_year/datazone_population_persons_singleyear_2018")
h5writeAttribute(did, name="Description", 
                 attr="Population of datazones in Scotland in 2018 contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary idnetifier columns/rows. Raw data available:  /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2018")
H5Dclose(did)
H5Fclose(fid)

