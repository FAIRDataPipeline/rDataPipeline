oa2grid <- function (dat, datazones, dz_subdivisions) 
{
  dat <- dat %>% tibble::column_to_rownames("OAcode")
  intersection_area <- data.frame(grid_id = dz_subdivisions$grid_id, 
                                  datazone = dz_subdivisions$OA11CD, area = as.numeric(sf::st_area(dz_subdivisions)))
  datazone_area <- data.frame(datazone = datazones$OA11CD, 
                              full_zone_area = as.numeric(sf::st_area(datazones)))
  combined_areas <- left_join(intersection_area, datazone_area, 
                              by = "datazone") %>% dplyr::mutate(proportion = area/full_zone_area) %>% 
    dplyr::select(grid_id, datazone, proportion) %>% dplyr::filter(datazone %in% 
                                                                     rownames(dat))
  wide_new_table <- reshape2::dcast(combined_areas, grid_id ~ 
                                      datazone, value.var = "proportion", fill = 0)
  prop_dat <- as.matrix(wide_new_table[-1])
  this_ageclass <- as.matrix(wide_new_table[-1])
  dzs <- colnames(this_ageclass)
  bins <- colnames(dat)
  grid_pop <- matrix(data = 0, nrow = nrow(this_ageclass), 
                     ncol = length(bins))
  colnames(grid_pop) <- bins
  for (j in seq_along(bins)) {
    for (i in seq_along(dzs)) {
      cat(paste0("\rAge class ", j, " of ", length(bins), 
                 "; Output area ", i, " of ", length(dzs), "..."))
      in_gridcell <- prop_dat[, i, drop = FALSE]
      true_pop <- dat[dzs[i], bins[j]]
      rounded_pops <- round(in_gridcell * true_pop)
      if (sum(rounded_pops) != true_pop) {
        non_rounded_pops <- in_gridcell * true_pop
        difference <- non_rounded_pops - rounded_pops
        remainder <- sum(non_rounded_pops) - sum(rounded_pops)
        if (remainder > 0) {
          next.biggest <- order(difference[, 1], decreasing = TRUE)[1:remainder]
          rounded_pops[next.biggest, 1] <- rounded_pops[next.biggest, 
                                                        1] + 1
        }
        if (remainder < 0) {
          next.biggest <- order((0 - difference[, 1]), 
                                decreasing = TRUE)[1:(0 - remainder)]
          rounded_pops[next.biggest, 1] <- rounded_pops[next.biggest, 
                                                        1] - 1
        }
      }
      this_ageclass[, i] <- rounded_pops
    }
    grid_pop[, bins[j]] <- rowSums(this_ageclass)
  }
  assertthat::assert_that(all(colSums(grid_pop) == colSums(dat)))
  list(grid_id = unlist(wide_new_table[, 1]), grid_pop = grid_pop)
}

