#' process_scotgov_ur_classification
#'
#' @export
#'
process_scotgov_ur_classification <- function(sourcefile, h5filename) {

  scotUR <- read.csv(file = sourcefile) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(featurecode = gsub(
      "<http://statistics.gov.scot/id/statistical-geography/", "",
      featurecode),
      featurecode = gsub(">", "", featurecode)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    dplyr::select(featurecode, rank) %>%
    dplyr::mutate(ur_name = dplyr::case_when(
      rank == 1 ~ "large_urban_area",
      rank == 2 ~ "other_urban_area",
      rank == 3 ~ "accessible_small_town",
      rank == 4 ~ "remote_small_town",
      rank == 5 ~ "accessible_rural",
      rank == 6 ~ "remote_rural"))

  create_table(h5filename = h5filename,
               component = "conversion",
               df = scotUR,
               row_title = "datazones",
               row_names = rownames(scotUR),
               column_units = colnames(scotUR))
}
