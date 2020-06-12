library(hdf5r)
library(SCRCdataAPI)
library(SPARQL)
library(dplyr)
library(magrittr)

h5filename <- "demographics_mini.h5"
file.h5 <- H5File$new(h5filename, mode = "r")
file.h5$ls(recursive = T) %$% name

names(file.h5[["grid10km/10year/persons"]])
names(file.h5[["grid10km/10year/persons"]])
file.h5[["grid10km/10year/persons/Dimension_1_names"]][]
file.h5[["grid10km/10year/persons/Dimension_1_title"]][]

file.h5[["grid10km/10year/persons/Dimension_2_names"]][]
file.h5[["grid10km/10year/persons/Dimension_2_title"]][]

file.h5[["grid10km/10year/persons/Dimension_1_values"]][]
file.h5[["grid10km/10year/persons/array"]][,]

file.h5$close_all()


