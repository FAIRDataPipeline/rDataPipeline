# 1
library(rhdf5)
library(readxl)


h5filename <- "scrc_demographics.h5"


# Create file
h5createFile(h5filename)

# Create group for raw data
h5createGroup(h5filename, "rawdata")
# Create subgroup for raw population data
h5createGroup(h5filename, "rawdata/scotlandpopulations")
# Create subgroup for SIMD deprivation data
h5createGroup(h5filename, "rawdata/SIMD")

# Create group for processed data
h5createGroup(h5filename, "processeddata")
# Create subgroup for processed population data
h5createGroup(h5filename, "processeddata/scotlandpopulations_single_year")

# Create group for grid data
h5createGroup(h5filename, "griddata")

# h5ls(h5filename) %>% View()




# Population data ---------------------------------------------------------

filenames <- dir("data-raw/population_datazone", full.names = TRUE) %>% 
        .[!grepl("edited", .)]


for(i in seq_along(filenames)) {
        cat("\nProcessing file", i, "of", length(filenames), "...\n")
        
        # Read raw data and export to h5 file
        sape_persons <- read_excel(filenames[i], col_names = FALSE)
        tag <- paste0("datazone_population_persons_singleyear_",
                      stringr::str_extract(filenames[i], "[0-9]+"))
        h5write(sape_persons, file = h5filename,
                name = file.path("rawdata/scotlandpopulations", tag))
        
        # Extract header
        header <- read_excel(filenames[i], skip = 3, n_max = 2) 
        # Rename first 4 columns
        header %<>% rename_at(vars(grep("^\\...[1-3]", names(.))), 
                              ~ as.character(header[2, 1:3])) %>% 
                rename(AllAges = "...4") %>% 
                names()
        
        # Process data and export to h5 file
        sape_persons %<>%
                # Remove first 6 rows
                .[-c(1:6),] %>% 
                # Rename columns
                rename_all(~header) %>% 
                # Remove empty columns (the 5th column)
                select_if(~sum(!is.na(.)) > 0) %>% 
                # Remove blank rows
                filter_all(any_vars(!is.na(.))) %>% 
                # Remove copyright
                filter_at(vars(ends_with("Code")), 
                          ~!grepl("Copyright", .)) %>%
                # Remove columns 2:4
                select_at(vars(-ends_with("Name"),
                               -AllAges)) %>% 
                mutate_at(vars(starts_with("AGE")), as.numeric)
        h5write(sape_persons, file = h5filename, 
                name = file.path("processeddata/scotlandpopulations_single_year",
                                 tag))
}




# Metadata/attributes -----------------------------------------------------

fid <- H5Fopen(h5filename)
years <- c(2013:2018)

# Raw data
for(i in seq_along(years)) {
        H5D_name <- file.path("/rawdata/scotlandpopulations",
                              paste0("datazone_population_persons_singleyear_",
                                     years[i]))
        did <- H5Dopen(fid, name = H5D_name) 
        H5D_attr <- paste("Population of datazones in Scotland in", years[i],
                           "contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series. Downloaded 24/4/20.")
        h5writeAttribute(did, name = "Description", attr = H5D_attr)
        H5Dclose(did)
}


# Processed data
for(i in seq_along(years)) {
        H5D_name <- file.path("/processeddata/scotlandpopulations_single_year",
                              paste0("datazone_population_persons_singleyear_",
                                     years[i]))
        did <- H5Dopen(fid, name = H5D_name) 
        H5D_attr <- paste("Population of datazones in Scotland in", years[i], "contains data in single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary identifier columns/rows. Raw data available: /rawdata/scotlandpopulations/datazone_population_persons_singleyear_", years[i])
        h5writeAttribute(did, name = "Description", attr = H5D_attr)
        H5Dclose(did)
}

H5Fclose(fid)

