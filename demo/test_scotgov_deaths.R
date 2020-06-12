library(hdf5r)
library(SCRCdataAPI)
library(SPARQL)

h5filename <- "deaths-involving-coronavirus-covid-19.h5"
file.h5 <- H5File$new(h5filename, mode = "r")
file.h5$ls(recursive = TRUE)

names(file.h5)
names(file.h5[["councilarea"]])
names(file.h5[["councilarea/per_week"]])
names(file.h5[["councilarea/per_week/all_deaths"]])

names(file.h5[["location"]])
names(file.h5[["location/per_week"]])
names(file.h5[["location/per_week/all_deaths"]])

names(file.h5[["scotland"]])
names(file.h5[["scotland/per_week"]])
names(file.h5[["scotland/per_week/all_deaths"]])
names(file.h5[["scotland/per_week/all_deaths/persons"]])
names(file.h5[["scotland/per_week/all_deaths/persons/all_ages"]])
names(file.h5[["scotland/per_week/all_deaths/persons/by_agegroup"]])
names(file.h5[["scotland/per_week/all_deaths/persons/averaged_over_5years"]])


file.h5[["scotland/per_week/all_deaths/persons/by_agegroup/array"]][,]
file.h5[["scotland/per_week/all_deaths/persons/by_agegroup/Dimension_1_names"]][]

file.h5[["scotland/per_week/all_deaths/persons/by_agegroup/array"]][,]



file.h5$close_all()


