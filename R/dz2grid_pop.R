#' dz2grid_pop
#' 
#' This script transforms age class population data from census geographies to 
#' a grid based system.
#' If the census geography is split across two grid cells this script splits
#' the population between these cells according to the proportion of the
#' postcodes in the geography which are present in each cell.
#' i.e. if a datazone containing 5 postcodes is split between 2 grid cells
#' with 3 postcodes in cell A and 2 in cell B, 60% of the population goes to cell A.
#' It takes:
#'   - shapefile of the census geographies from which the populations should
#'   be drawn
#'   - a population dataset containing single-year age classes for each
#'   census geography (alternatively a population dataset with preset age
#'   classes can be used) in the structure: 1st column - Geographical identifier,
#'   columns 2:92 single year population totals for age 0 to 89 and 90+ ageclass
#'   - A grid shapefile which overlays the census geographies. (IF this is not
#'    available this script automatically makes a grid, the dimensions of which
#'     can be chosen (in m))
#'     - A shapefile containing the boundaries of postcodes contained in the
#'     census geographies
#'     
#'     If a single-year age class dataset is supplied and a different structure is
#'     desired (i.e. 5-year age classes) the structure of this new age class must
#'     be set as a vector called: age_class_structure
#'     This vector should contain the lower age bound of each age class
#'     
#'     There is an issue of rounding when the population are being divided between
#'     cells, I have a work-around which is explained in the code.
#' 
#' 
#' @param 
#' 
dz2grid_pop <- function(pop_datazone, 
                        datazone_sf,
                        postcode_sf,
                        grid_size = 10000) {
  # Age class agreggation ---------------------------------------------------
  
  # Remove empty datazones ("S01010206", "S01010226", and "S01010227")
  pop_datazone %<>% 
    filter(rowSums(select(., -datazone)) != 0)
  
  datazone_populations <- pop_datazone %>% 
    mutate(AllAges = rowSums(select(., -datazone))) %>% 
    select(datazone, AllAges) 
  
  # Read in datazone shapefile and check for non-intersecting geometries
  shape <- st_read(datazone_sf)
  check <- sf::st_is_valid(shape, reason = TRUE)
  if(any(check != "Valid Geometry")) {
    datazones <- st_make_valid(shape)
    assertthat::assert_that(sum(st_area(shape)) == sum(st_area(datazones)))
  } else 
    datazones <- shape
  
  # Datazone-grid conversion -----------------------------------------------
  
  # Generate grid over bounding box of datazone shapefile
  grids <- st_make_grid(st_as_sfc(st_bbox(datazones)), 
                        cellsize = c(grid_size, grid_size)) %>% 
    st_sf(grid_id = seq_along(.))
  
  # Use grid to subdivide datazones
  dz_subdivisions <- st_intersection(grids, datazones)
  
  # Read in postcode shapefile 
  postcode <- st_read(postcode_sf) 
  
  
  # Find the total number of postcodes in each datazone ---------------------
  
  dz_postcode <- postcode 
  st_geometry(dz_postcode) <- NULL
  
  dz_postcode_table <- dz_postcode %>% 
    rename(datazone = DZ11) %>% 
    select(Postcode, datazone) %>%  
    unique() %>% 
    group_by(datazone, .drop = FALSE) %>%
    # using postcode shapefile datazones
    summarise(postcodes_in_dz = n()) 
  
  
  # Find the total number of postcodes in each dz_subdivision ---------------
  
  dz_grid_postcode <- st_join(postcode, dz_subdivisions) 
  
  # Remove PA75 6NUB (it doesnt exist within postcode shapefile datazones)
  dz_grid_postcode %<>% filter(!is.na(DataZone))  
  
  st_geometry(dz_grid_postcode) <- NULL
  dz_grid_postcode %<>% select(Postcode, DZ11, grid_id) %>% unique()
  
  dz_grid_postcode_table <- dz_grid_postcode %>% 
    rename(datazone = DZ11) %>%  
    group_by(datazone, grid_id, .drop = FALSE) %>% 
    # using postcode shapefile datazones
    summarise(postcodes_in_dz_component = n()) 
  
  
  # Calculate the proportion of postcodes in each dz_subdivision ------------
  # (relative to dz)
  postcode_prop <- left_join(dz_grid_postcode_table, 
                             dz_postcode_table, by = "datazone") %>% 
    mutate(proportion = postcodes_in_dz_component / postcodes_in_dz)
  
  # Find the sum of the proportions within each dz
  combined_areas <- postcode_prop %>% 
    select(datazone, grid_id, proportion) 
  combined_areas_total_prop <- combined_areas %>% 
    group_by(datazone, .drop = FALSE) %>%
    summarise(sum = sum(proportion))
  
  # This value is greater than 1 when postcodes exist in multiple grids. 
  # To prevent this, normalise within each dz
  combined_areas <- left_join(combined_areas, combined_areas_total_prop, 
                              by = "datazone") %>% 
    mutate(proportion2 = proportion / sum) %>% 
    select(datazone, grid_id, proportion2)
  
  # Create matrix containing the proportion of postcodes in each dz_subdivision 
  wide_new_table <- tidyr::pivot_wider(combined_areas, 
                                       names_from = "datazone",
                                       values_from = "proportion2", 
                                       values_fill = list("proportion2" = 0)) %>% 
    select(grid_id, datazone_populations$datazone)
  
  # Make new tables to fill in population proportions
  prop_dat <- wide_new_table %>% 
    select(-grid_id)
  
  output <- wide_new_table %>% 
    data.frame() %>% 
    tibble::column_to_rownames("grid_id")
  
  # Loop over each row (grid_id) and find the proportion of the population 
  # of each datazone in each grid cell 
  
  for(i in seq_len(ncol(output))) {
    
    # Find non empty datazone-grid components
    non_empty <- which(is.na(prop_dat[, i]) == FALSE)
    in_gridcell <- prop_dat[non_empty, i]
    
    # Total population count in each datazone
    dz_total <- datazone_populations %>% 
      dplyr::filter(datazone == colnames(in_gridcell)) %$%
      AllAges
    
    rounded_pops <- round(in_gridcell*dz_total)
    
    # Work around for the rounding issue:
    # If the rounded population is less than the true population, calculate 
    # which areas are the closest to the next integer. Add 1 individual to each 
    # of the closest areas until the total is met. Conversely, if the rounding 
    # causes a higher population than expected, remove individuals from the 
    # grid cells furthest from the nearest integer.
    
    if(sum(rounded_pops) != sum(dz_total)) {
      non_rounded_pops <- in_gridcell * dz_total
      difference <- non_rounded_pops - rounded_pops
      remainder <- sum(non_rounded_pops) - sum(rounded_pops)
      if(remainder > 0) {
        next.biggest <- order(difference[, 1], decreasing = TRUE)[1:remainder]
        rounded_pops[next.biggest, 1] <- rounded_pops[next.biggest, 1] + 1
      }
      if(remainder < 0) {
        next.biggest <- order((0 - difference[, 1]), 
                              decreasing = TRUE)[1:(0-remainder)]
        rounded_pops[next.biggest, 1] <- rounded_pops[next.biggest, 1] - 1
      }
    }
    output[non_empty, i] <- rounded_pops
    assertthat::assert_that(sum(rounded_pops) == sum(dz_total))
  }
  
  # Check that redistributed population size matches original population size
  assertthat::assert_that(all(colSums(output) == 
                                datazone_populations$AllAges))
  
  # Sum across datazones to find total population in each grid cell
  output %>% 
    tibble::rownames_to_column("grid_id") %>% 
    dplyr::mutate(population = rowSums(select(., -grid_id))) %>% 
    dplyr::select(grid_id, population)
}