#' dz2grid
#' 
dz2grid <- function(wide_new_table, 
                    datazone_populations, 
                    age_class_tags, 
                    ageclasses, 
                    datazones,
                    dz_subdivisions) {
  
  num_ageclasses <- length(ageclasses)
  
  # Make new tables to fill in population proportions
  prop_dat <- wide_new_table[-1] %>% 
    as_tibble()
  
  assertthat::assert_that(!any(is.na(prop_dat)))
  
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
      
      in_gridcell <- prop_dat[,i]
      
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
      
      this_ageclass[, i] <- rounded_pops
      
    }
    
    # Sum across datazones to find total population in each grid cell
    grid_pop[, as.character(ageclasses[j])] <- rowSums(this_ageclass)
  }
  
  # Check age group counts
  assertthat::assert_that(all(colSums(grid_pop[,-1, drop = F]) == 
                                colSums(datazone_populations)))
  
  colnames(grid_pop) <- c("grid_id", age_class_tags)
  as.data.frame(grid_pop)
}