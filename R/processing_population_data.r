# 1

h5filename <- "scrc_demographics.h5"


# Create hdf5 file
rhdf5::h5createFile(h5filename)

# Create group for datazones and subgroup for scotland2018
rhdf5::h5createGroup(h5filename, "datazone")
rhdf5::h5createGroup(h5filename, "datazone/scotland2018")

# Create group for grid data
rhdf5::h5createGroup(h5filename, "griddata")



# Population data ---------------------------------------------------------

filename <- "data-raw/population_datazone/sape-2018-persons.xlsx"

# Read raw data and export to h5 file
sape_persons <- readxl::read_excel(filename, col_names = FALSE)

# Extract header
header <- readxl::read_excel(filename, skip = 3, n_max = 2) 
# Rename first 4 columns
header %<>% dplyr::rename_at(vars(grep("^\\...[1-3]", names(.))), 
                      ~ as.character(header[2, 1:3])) %>% 
        dplyr::rename(AllAges = "...4") %>% 
        names()

# Process data and export to h5 file
sape_persons %<>%
        # Remove first 6 rows
        .[-c(1:6),] %>% 
        # Rename columns
        dplyr::rename_all(~header) %>% 
        # Remove empty columns (the 5th column)
        dplyr::select_if(~sum(!is.na(.)) > 0) %>% 
        # Remove blank rows
        dplyr::filter_all(any_vars(!is.na(.))) %>% 
        # Remove copyright
        dplyr::filter_at(vars(dplyr::ends_with("Code")), 
                  ~!grepl("Copyright", .)) %>%
        # Remove columns 2:4
        dplyr::select_at(vars(-dplyr::ends_with("Name"),
                       -AllAges)) %>% 
        dplyr::mutate_at(vars(dplyr::starts_with("AGE")), as.numeric)

h5write(sape_persons, file = h5filename, 
        name = file.path("datazone/scotland2018"))


# Metadata/attributes -----------------------------------------------------

fid <- H5Fopen(h5filename)

H5D_name <- file.path("/datazone/scotland2018")
did <- H5Dopen(fid, name = H5D_name) 
H5D_attr <- paste("Population of datazones in Scotland in 2018 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages, data from National Records Scotland, available at https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series. Downloaded 24/4/20. Processed to remove unnecessary identifier columns/rows. Raw data available: /rawdata/scotlandpopulations/datazone_population_persons_singleyear_2018")
h5writeAttribute(did, name = "Description", attr = H5D_attr)
H5Dclose(did)

H5Fclose(fid)

