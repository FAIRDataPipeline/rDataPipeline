# 1



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
        name = file.path("scotland2018/datazone"))


# Metadata/attributes -----------------------------------------------------

# Open file
fid <- H5Fopen(h5filename)

# Add some attributes to the group
gid <- H5Gopen(fid, name = "scotland2018/") 

h5writeAttribute(did, attr = "Population of datazones in Scotland in 2018 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary identifier columns/rows.", 
                 name = "Description")
h5writeAttribute(did, attr = "24/4/20", name = "DownloadDate")
h5writeAttribute(did, attr = "1", name = "RawWarningScore")
h5writeAttribute(did, attr = "National Records Scotland", name = "Source")
h5writeAttribute(did, attr = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series", name = "URL")

# Add some attributes to the dataset (datazone)
did <- H5Dopen(fid, "scotland2018/datazone")

h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")

# Close file, groups, and datasets
H5Fclose(fid)
H5Fclose(gid)
H5Dclose(did)


