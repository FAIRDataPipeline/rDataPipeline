#' process_scotgov_simd_income
#'
#' @export
#'
process_scotgov_simd_income <- function(sourcefile, h5filename) {

  scotSIMDinc <- read.csv(file = sourcefile) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(featurecode = gsub(
      "<http://statistics.gov.scot/id/statistical-geography/", "",
      featurecode),
      featurecode = gsub(">", "", featurecode))

}
