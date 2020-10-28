#' Bin ages
#'
#' Function used to bin ages in a \code{data.frame}
#'
#' @param dat a \code{data.frame} containing population counts (see Examples
#' below)
#' @param ageclasses a \code{data.frame} containing age class boundaries
#'
#' @export
#'
#' @examples
#' # NRS example
#' df <- matrix(sample(273), ncol = 91)
#' colnames(df) <- paste0("AGE", 0:90)
#' colnames(df)[91] <- "AGE90+"
#' bin_ages(df, c(0, 10, 50, 80))
#' bin_ages(df, "total")
#'
bin_ages <- function(dat,
                     ageclasses) {

  if(is.matrix(dat))
    dat <- as.data.frame(dat)

  if(all(ageclasses == "total")) {
    output <- data.frame(total = rowSums(dat))
    return(output)

  } else {
    # Find total number of individuals in each age class
    output <- matrix(data = 0, ncol = length(ageclasses),
                     nrow = nrow(dat))

    for(i in seq_along(ageclasses)) {

      maxage <- colnames(dat)[ncol(dat)]
      maxage <- gsub("AGE", "", maxage)
      maxage <- gsub("\\+", "", maxage) %>%
        as.numeric()
      endcol <- dplyr::if_else(i == length(ageclasses), maxage,
                               (ageclasses[i + 1] - 1))
      columns <- paste0("AGE", ageclasses[i]:endcol)

      if("AGE90+" %in% colnames(dat))
        dat <- dplyr::rename(dat, AGE90 = .data$`AGE90+`)

      output[,i] <- dat %>%
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
    return(output)
  }
}
