#' process_population
#' 
#' @param path path to demographic data input file (datazones)
#' 
process_population <- function(path) {
  # Read raw data
  sape_persons <- readxl::read_excel(path, col_names = FALSE)
  
  # Extract header
  header <- readxl::read_excel(path, skip = 3, n_max = 2) 
  
  # Rename first 4 columns
  header %<>% dplyr::rename_at(vars(grep("^\\...[1-3]", names(.))), 
                               ~ as.character(header[2, 1:3])) %>% 
    dplyr::rename(AllAges = "...4") %>% 
    names()
  
  # Process data
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
}