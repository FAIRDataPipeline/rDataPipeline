#' bin_ages
#'
bin_ages <- function(population_dz, ageclasses) {

  # Remove empty datazones ("S01010206", "S01010226", and "S01010227")
  dat <- population_dz %>%
    dplyr::filter(rowSums(dplyr::select(., -datazone)) != 0)

  if(all(ageclasses == "AllAges")) {
    datazone_pop <- dat %>%
      dplyr::mutate(AllAges = rowSums(dplyr::select(., -datazone))) %>%
      dplyr::select(datazone, AllAges) %>%
      tibble::column_to_rownames("datazone")

  } else {
    # Find total number of individuals in each age class
    datazone_pop <- matrix(data = 0, ncol = length(ageclasses),
                                   nrow = nrow(dat))

    for(i in seq_along(ageclasses)) {

      endcol <- dplyr::if_else(i == length(ageclasses), max(ageclasses),
                               (ageclasses[i + 1] - 1))
      columns <- paste0("AGE", ageclasses[i]:endcol)

      datazone_pop[,i] <- dat %>%
        dplyr::rename(AGE90 = "AGE90+") %>%
        dplyr::select(dplyr::one_of(columns)) %>%
        rowSums()
    }
    rownames(datazone_pop) <- dat$datazone

    tag_ageclass <- lapply(seq_along(ageclasses), function(x)
      if(x != length(ageclasses)) {
        paste0(ageclasses[x], "-", ageclasses[x+1]-1)
      } else {
        paste0(ageclasses[x], "+")
      }
    ) %>% unlist()

    colnames(datazone_pop) <- tag_ageclass

  }

  datazone_pop
}
