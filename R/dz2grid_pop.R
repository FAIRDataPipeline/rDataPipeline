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
#' @param method c("postcode", "area")
#' @param ageclasses vector of class numeric corresponding to the lower bound
#' of each age class; when missing, a single age class is generated (all 
#' ages combined)
#' @param datazones object of class sf and data.frame
#' @param dz_subdivisions object of class sf and data.frame
#' @param postcode path to postcode shape file 
#' 
dz2grid_pop <- function(population_dz, 
                        method,
                        ageclasses,
                        datazones,
                        dz_subdivisions,
                        postcode) {
  
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
  
  
  if(method == "postcode") {
    # Find the total number of postcodes in each datazone ---------------------
    
    dz_postcode <- postcode 
    sf::st_geometry(dz_postcode) <- NULL
    
    dz_postcode_table <- dz_postcode %>% 
      dplyr::rename(datazone = DZ11) %>% 
      dplyr::select(Postcode, datazone) %>%  
      unique() %>% 
      dplyr::group_by(datazone, .drop = FALSE) %>%
      # using postcode shapefile datazones !!!
      dplyr::summarise(postcodes_in_dz = n()) 
    
    
    # Find the total number of postcodes in each dz_subdivision ---------------
    
    dz_grid_postcode <- sf::st_join(postcode, dz_subdivisions) 
    
    # Remove PA75 6NUB (it doesnt exist within postcode shapefile datazones)
    dz_grid_postcode <- dz_grid_postcode %>% 
      dplyr::filter(!is.na(DataZone))  
    sf::st_geometry(dz_grid_postcode) <- NULL
    
    dz_grid_postcode <- dz_grid_postcode %>% 
      dplyr::select(Postcode, DZ11, grid_id) %>%
      unique()
    
    dz_grid_postcode_table <- dz_grid_postcode %>% 
      dplyr::rename(datazone = DZ11) %>%  
      dplyr::group_by(datazone, grid_id, .drop = FALSE) %>% 
      # using postcode shapefile datazones !!!
      dplyr::summarise(postcodes_in_dz_component = n()) 
    
    
    # Calculate the proportion of postcodes in each dz_subdivision ------------
    # (relative to dz)
    postcode_prop <- dplyr::left_join(dz_grid_postcode_table, 
                                      dz_postcode_table, by = "datazone") %>% 
      dplyr::mutate(proportion = postcodes_in_dz_component / postcodes_in_dz)
    
    # Find the sum of the proportions within each dz
    combined_areas <- postcode_prop %>% 
      dplyr::select(datazone, grid_id, proportion) 
    combined_areas_total_prop <- combined_areas %>% 
      dplyr::group_by(datazone, .drop = FALSE) %>%
      dplyr::summarise(sum = sum(proportion))
    
    # This value is greater than 1 when postcodes exist in multiple grids. 
    # To prevent this, normalise within each dz
    combined_areas <- dplyr::left_join(combined_areas, 
                                       combined_areas_total_prop, 
                                       by = "datazone") %>% 
      dplyr::mutate(proportion2 = proportion / sum) %>% 
      dplyr::select(datazone, grid_id, proportion2)
    
    # Create matrix containing the proportion of postcodes in each dz_subdivision 
    wide_new_table <- tidyr::pivot_wider(combined_areas, 
                                         names_from = "datazone",
                                         values_from = "proportion2", 
                                         values_fill = list("proportion2" = 0)) %>% 
      # using datazone shapefile datazones !!!
      dplyr::select(grid_id, rownames(datazone_populations))
    
  } else if(method == "area") {
    
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
    
  } else 
    stop("Method not valid.")
  
  
  # Make new tables to fill in population proportions
  prop_dat <- wide_new_table[-1] %>% 
    as_tibble()
  
  this_ageclass <- wide_new_table[-1] %>% 
    as_tibble()
  dzs <- colnames(this_ageclass)
  
  grid_pop <- matrix(data = 0, nrow = nrow(this_ageclass),
                     ncol = (num_ageclasses + 1))
  
  grid_pop[,1] <- unlist(wide_new_table[,1])
  colnames(grid_pop) <- c("grid_id", ageclasses)
  
  # Loop over each row (grid_id) and find the proportion of the population 
  # of each datazone in each grid cell 
  
  for(j in seq_along(ageclasses)) {
    
    for(i in seq_along(dzs)) {
      cat(paste0('\rAge class ', j, " of ", length(seq_along(ageclasses)), 
                 "; datazone ", i, " of ", 
          ncol(this_ageclass), "..."))
      
      in_gridcell <- prop_dat[which(is.na(prop_dat[,i])==FALSE),i]
      
      true_pop <- datazone_populations[dzs[i], as.character(ageclasses[j])]
      rounded_pops <- round(in_gridcell * true_pop)
      
      # Work around for the rounding issue:
      # If the rounded population is less than the true population, calculate 
      # which areas are the closest to the next integer. Add 1 individual to each 
      # of the closest areas until the total is met. Conversely, if the rounding 
      # causes a higher population than expected, remove individuals from the 
      # grid cells furthest from the nearest integer.
      
      if(sum(rounded_pops) != sum(true_pop)) {
        non_rounded_pops <- in_gridcell * true_pop
        difference <- non_rounded_pops - rounded_pops
        remainder <- sum(non_rounded_pops) - sum(rounded_pops)
        if(remainder > 0){
          next.biggest <- order(difference[, 1], decreasing = TRUE)[1:remainder]
          rounded_pops[next.biggest,1] <- rounded_pops[next.biggest, 1] + 1
        }
        if(remainder < 0){
          next.biggest <- order((0-difference[,1]), 
                                decreasing = TRUE)[1:(0-remainder)]
          rounded_pops[next.biggest,1] <- rounded_pops[next.biggest, 1] - 1
        }
      }
      
      this_ageclass[which(is.na(prop_dat[,i]) == FALSE), i] <- rounded_pops
      
    }
    
    # Sum across datazones to find total population in each grid cell
    grid_pop[, as.character(ageclasses[j])] <- rowSums(this_ageclass)
  }
  
  # Check age group counts
  assertthat::assert_that(all(colSums(grid_pop[,-1, drop = F]) == 
                                colSums(datazone_populations)))
  
  # Check that redistributed population size matches datazone population size 
  # assertthat::assert_that(sum(this_ageclass[,i]) == 
  #                           datazone_populations %>%
  #                           filter(datazone == ))
  
  
  colnames(grid_pop) <- c("grid_id", age_class_tags)
  as.data.frame(grid_pop)
}