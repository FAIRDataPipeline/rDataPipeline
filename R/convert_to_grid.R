#' Convert census geographies to grid based system
#'
#' This script transforms age class population data from census geographies to
#' a grid based system.
#'
#' @param dat population data
#' @param shapefile shapefile
#' @param subdivisions subdivisions
#' @param conversion.table conversion table
#' @param grid_size grid size
#'
#' @export
#'
convert_to_grid <- function(dat,
                            shapefile,
                            subdivisions,
                            conversion.table,
                            grid_size) {
  dat <- dat %>% tibble::column_to_rownames("AREAcode")

  conversion.table <- conversion.table %>% tibble() %>%
    dplyr::select(colnames(conversion.table)[grepl(grid_size,
                                                   colnames(conversion.table))],
                  .data$AREAcode) %>%
    dplyr::filter(.data$AREAcode %in% rownames(dat)) %>%
    dplyr::rename(grid_id = paste0(grid_size,"_id"),
                  proportion = paste0(grid_size,"_area_proportion"))

  if(grid_size != "grid1km") {
    ind <- which(duplicated(conversion.table[c("grid_id",
                                               "AREAcode")]) == FALSE)
    conversion.table <- conversion.table[ind,]
  }

  # Create matrix of grid cells by shapefile containing the proportion of
  # each datazone in each grid cell with 0's
  # wide_new_table <- reshape2::dcast(conversion.table, grid_id ~ AREAcode,
  #                                   value.var = "proportion", fill = 0)

  # Make new tables to fill in population proportions
  # prop_dat <- as.matrix(wide_new_table[-1])
  # this_ageclass <- as.matrix(wide_new_table[-1])

  dzs <- unique(conversion.table$AREAcode)
  bins <- colnames(dat)
  grids <- unique(conversion.table$grid_id)

  grid_pop <- matrix(data = 0, nrow = length(grids),
                     ncol = length(bins))
  colnames(grid_pop) <- bins
  rownames(grid_pop) <- grids

  # Loop over each row (grid_id) and find the proportion of the population
  # of each datazone in each grid cell

  for(j in seq_along(bins)) {
    #Attempt to make faster
    age_class_dat <- dat %>% select(bins[j])
    for(i in seq_along(dzs)) {

      in_gridcell <-   conversion.table[conversion.table$AREAcode==dzs[i],]
      true_pop <- age_class_dat[dzs[i],]
      rounded_pops <- round(in_gridcell$proportion * true_pop)

      # Work around for the rounding issue:
      # If the rounded population is less than the true population, calculate
      # which areas are the closest to the next integer. Add 1 individual to each
      # of the closest areas until the total is met. Conversely, if the rounding
      # causes a higher population than expected, remove individuals from the
      # grid cells furthest from the nearest integer.

      if(sum(rounded_pops) != true_pop) {
        non_rounded_pops <- in_gridcell$proportion * true_pop
        difference <- non_rounded_pops - rounded_pops
        remainder <- sum(non_rounded_pops) - sum(rounded_pops)

        if(remainder > 0){
          # Find cells to adjust
          next.biggest <- order(abs(difference),
                                decreasing = TRUE)[1:abs(remainder)]

          # Break any ties: choose randomly to avoid a systematic bias by cell order
          tied_first <- difference %in% difference[next.biggest]

          if(!isTRUE(all.equal(sum(tied_first), abs(remainder)))) {
            next.biggest <- sample(which(tied_first), size = round(abs(remainder)))
          }

          # Make adjustment

          rounded_pops[next.biggest] <- rounded_pops[next.biggest] + 1
        }
        if(remainder < 0){
          # This is calculated differently when we need to remove "individuals"
          # as the individual should be taken from the grid cell with the
          # smallest proprotional area of the gird cells that already have
          # populations
          difference <-  (non_rounded_pops*rounded_pops) - rounded_pops

          # Find cells to adjust
          next.biggest <- order(abs(difference),
                                decreasing = FALSE)[which(sort(abs(difference))!=0)][1:abs(remainder)]

          # Break any ties: choose randomly to avoid a systematic bias by cell order
          tied_first <- difference %in% difference[next.biggest]

          if(!isTRUE(all.equal(sum(tied_first), abs(remainder)))) {
            next.biggest <- sample(which(tied_first), size = round(abs(remainder)))
          }

          # Make adjustment
          rounded_pops[next.biggest] <- rounded_pops[next.biggest] - 1
        }
      }
      assertthat::assert_that(all(rounded_pops>=0))
      grid_pop[in_gridcell$grid_id, bins[j]] = grid_pop[in_gridcell$grid_id, bins[j]] +rounded_pops
    }

    # Sum across shapefile to find total population in each grid cell
    # grid_pop[, bins[j]] <- rowSums(this_ageclass)
  }

  # Check age group counts
  assertthat::assert_that(all(colSums(grid_pop) == colSums(dat)))

  list(grid_id = grids,
       grid_pop = grid_pop)

}
