#' bin_ages
#'
bin_ages <- function(dat, ageclasses) {

  if(all(ageclasses == "total")) {
    output <- dat %>%
      dplyr::mutate(total = rowSums(dplyr::select(., -DZcode))) %>%
      dplyr::select(DZcode, total)

  } else {
    # Find total number of individuals in each age class
    output <- matrix(data = 0, ncol = length(ageclasses),
                         nrow = nrow(dat))

    for(i in seq_along(ageclasses)) {

      maxage <- colnames(dat)[ncol(dat)] %>%
        gsub("AGE", "", .) %>%
        gsub("\\+", "", .) %>%
        as.numeric()
      endcol <- dplyr::if_else(i == length(ageclasses), maxage,
                               (ageclasses[i + 1] - 1))
      columns <- paste0("AGE", ageclasses[i]:endcol)

      output[,i] <- dat %>%
        dplyr::rename(AGE90 = "AGE90+") %>%
        dplyr::select(dplyr::one_of(columns)) %>%
        rowSums()
    }

    tag_ageclass <- lapply(seq_along(ageclasses), function(x)
      if(x != length(ageclasses)) {
        paste0(ageclasses[x], "-", ageclasses[x+1]-1)
      } else {
        paste0(ageclasses[x], "+")
      }
    ) %>% unlist()

    colnames(output) <- paste0("AGE", tag_ageclass)

    output <- cbind.data.frame(AREAcode = dat$AREAcode, output)
  }

  output
}
