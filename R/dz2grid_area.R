#' dz2grid_area
#' 
#' This script transforms age class population data from census geographies to 
#' a grid based system.
#' If the census geography is split across two grid cells this script splits
#' the population between these cells according to the proportion of the
#' postcodes in the geography which are present in each cell.
#' i.e. if a datazone containing 5 postcodes is split between 2 grid cells
#' with 3 postcodes in cell A and 2 in cell B, 60% of the population goes to cell A.
#' It takes:
#' - shapefile of the census geographies from which the populations should
#' be drawn
#' - a population dataset containing single-year age classes for each
#' census geography (alternatively a population dataset with preset age
#' classes can be used) in the structure: 1st column - Geographical identifier,
#' columns 2:92 single year population totals for age 0 to 89 and 90+ ageclass
#' - A grid shapefile which overlays the census geographies. (IF this is not
#' available this script automatically makes a grid, the dimensions of which
#' can be chosen (in m))
#' - A shapefile containing the boundaries of postcodes contained in the
#' census geographies
#'   
#' If a single-year age class dataset is supplied and a different structure is
#' desired (i.e. 5-year age classes) the structure of this new age class must
#' be set as a vector called: age_class_structure
#' This vector should contain the lower age bound of each age class
#'     
#' There is an issue of rounding when the population are being divided between
#' cells, I have a work-around which is explained in the code.
#' 
#' @param population_dz datazone population data
#' @param ageclasses vector of class numeric corresponding to the lower bound
#' of each age class; when missing, a single age class is generated (all 
#' ages combined)
#' @param datazones 
#' @param dz_subdivisions 
#' 
dz2grid_area <- function(population_dz, 
                         ageclasses,
                         datazones,
                         dz_subdivisions) {
  
  # Remove empty datazones ("S01010206", "S01010226", and "S01010227")
  dat <- population_dz %>% 
    dplyr::filter(rowSums(dplyr::select(., -datazone)) != 0)
  
  
  # Age class agreggation ---------------------------------------------------
  
  if(missing(ageclasses)) {
    ageclasses <- "AllAges"
    num_ageclasses <- length(ageclasses)
    
    datazone_populations <- dat %>% 
      dplyr::mutate(AllAges = rowSums(dplyr::select(., -datazone))) %>% 
      dplyr::select(datazone, AllAges) %>% 
      tibble::column_to_rownames("datazone")
    
    age_class_tags <- "AllAges"
    
  } else {
    num_ageclasses <- length(ageclasses)
    
    # Find total number of individuals in each age class
    
    datazone_populations <- matrix(data = 0, ncol = num_ageclasses,
                                   nrow = nrow(dat))
    
    for(i in seq_along(ageclasses)) {
      
      endcol <- dplyr::if_else(i == length(ageclasses), max(ageclasses), 
                               (ageclasses[i + 1] - 1))
      columns <- paste0("AGE", ageclasses[i]:endcol)
      
      datazone_populations[,i] <- dat %>% 
        dplyr::rename(AGE90 = "AGE90+") %>% 
        dplyr::select(dplyr::one_of(columns)) %>% 
        rowSums()
    }
    colnames(datazone_populations) <- ageclasses
    rownames(datazone_populations) <- dat$datazone
    
    age_class_tags <- lapply(seq_along(ageclasses), function(x) 
      if(x != length(ageclasses)) {
        paste0(ageclasses[x], "-", ageclasses[x+1]-1)
      } else {
        paste0(ageclasses[x], "+")
      }
    ) %>% unlist()
  }
  
  # Find area of each of the newly created distinct datazone components
  intersection_area <- data.frame(grid_id = dz_subdivisions$grid_id,
                                  datazone = dz_subdivisions$DataZone, 
                                  area = as.numeric(sf::st_area(dz_subdivisions)))
  datazone_area <- data.frame(datazone = datazones$DataZone, 
                              full_zone_area = as.numeric(sf::st_area(datazones)))
  
  # Join full area of each datazone to this data.frame and use to find 
  # the proportion of each datazone in each grid cell
  combined_areas <- left_join(intersection_area, datazone_area, 
                              by = "datazone") %>% 
    dplyr::mutate(proportion = area / full_zone_area) %>% 
    dplyr::select(grid_id, datazone, proportion) %>% 
    dplyr::filter(datazone %in% rownames(datazone_populations))
  
  # Create matrix of grid cells by datazones containing the proportion of 
  # each datazone in each grid cell with 0's
  wide_new_table <- tidyr::pivot_wider(combined_areas, 
                                       names_from = "datazone", 
                                       values_from = "proportion") %>%
    replace(is.na(.), 0)
  
  dz2grid(wide_new_table,
          datazone_populations, 
          age_class_tags, 
          ageclasses, 
          datazones,
          dz_subdivisions)
}