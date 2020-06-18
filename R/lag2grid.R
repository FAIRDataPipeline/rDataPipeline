#' dz2grid
#'
#' This script transforms age class population data from census geographies to
#' a grid based system.
#'
#' @param dat datazone population data
#' @param datazones datazones
#' @param dz_subdivisions datazone subdivisions
#'
dz2grid <- function(dat,
                    LAG,
                    lag_subdivisions) {

  dat <- dat %>% tibble::column_to_rownames("LAGcode")

  # Find area of each of the newly created distinct datazone components
  intersection_area <- data.frame(grid_id = lag_subdivisions$grid_id,
                                  LAGs = lag_subdivisions$LAG,
                                  area = as.numeric(sf::st_area(lag_subdivisions)))
  lag_area <- data.frame(LAGs = LAG$LAG,
                              full_zone_area = as.numeric(sf::st_area(LAG)))

  # Join full area of each datazone to this data.frame and use to find
  # the proportion of each datazone in each grid cell
  combined_areas <- left_join(intersection_area, lag_area,
                              by = "LAGs") %>%
    dplyr::mutate(proportion = area / full_zone_area) %>%
    dplyr::select(grid_id, LAGs, proportion) %>%
    dplyr::filter(LAGs %in% rownames(dat))

  # Create matrix of grid cells by datazones containing the proportion of
  # each datazone in each grid cell with 0's
  wide_new_table <- reshape2::dcast(combined_areas, grid_id ~ LAGs,
                                    value.var = "proportion", fill = 0)

  # Make new tables to fill in population proportions
  prop_dat <- as.matrix(wide_new_table[-1])
  this_ageclass <- as.matrix(wide_new_table[-1])

  dzs <- colnames(this_ageclass)
  bins <- colnames(dat)

  grid_pop <- matrix(data = 0, nrow = nrow(this_ageclass),
                     ncol = length(bins))
  colnames(grid_pop) <- bins

  # Loop over each row (grid_id) and find the proportion of the population
  # of each datazone in each grid cell

  for(j in seq_along(bins)) {

    for(i in seq_along(dzs)) {
      cat(paste0('\rAge class ', j, " of ", length(bins),
                 "; LAG ", i, " of ",
                 length(dzs), "..."))

      in_gridcell <- prop_dat[, i, drop = FALSE]
      true_pop <- dat[dzs[i], bins[j]]
      rounded_pops <- round(in_gridcell * true_pop)

      # Work around for the rounding issue:
      # If the rounded population is less than the true population, calculate
      # which areas are the closest to the next integer. Add 1 individual to each
      # of the closest areas until the total is met. Conversely, if the rounding
      # causes a higher population than expected, remove individuals from the
      # grid cells furthest from the nearest integer.

      if(sum(rounded_pops) != true_pop) {
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
    grid_pop[, bins[j]] <- rowSums(this_ageclass)
  }

  # Check age group counts
  assertthat::assert_that(all(colSums(grid_pop) ==
                                colSums(dat)))

  list(grid_id = unlist(wide_new_table[,1]),
       grid_pop = grid_pop)
}
