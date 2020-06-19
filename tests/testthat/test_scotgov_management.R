library(hdf5r)
library(SCRCdataAPI)
library(SPARQL)

h5filename <- "coronavirus-covid-19-management-information.h5"
file.h5 <- H5File$new(h5filename, mode = "r")
file.h5$ls(recursive = TRUE)

names(file.h5[["scotland/testing/cumulative/people_tested"]])
file.h5[["scotland/testing/cumulative/people_tested/array"]][,]
file.h5[["scotland/testing/cumulative/people_tested/Dimension_1_names"]][]
file.h5[["scotland/testing/cumulative/people_tested/Dimension_2_names"]][]

tmp <- read_array(h5filename, "scotland/testing/cumulative/people_tested")
ncol(tmp)

tmp[,100:108] %>% View()


file.h5$close_all()


