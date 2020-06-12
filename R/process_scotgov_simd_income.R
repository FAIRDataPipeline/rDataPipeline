#' process_scotgov_simd_income
#'
#' @export
#'
process_scotgov_simd_income <- function(sourcefile, h5filename) {

  scotSIMDinc <- lapply(seq_along(sourcefile), function(i) {
    read.csv(file = sourcefile[i]) %>%
      dplyr::select(-X) %>%
      dplyr::mutate(featurecode = gsub(
        "<http://statistics.gov.scot/id/statistical-geography/", "",
        featurecode),
        featurecode = gsub(">", "", featurecode))
  }) %>% do.call(rbind.data.frame, .) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ measuretype, value.var = "values") %>%
    tibble::column_to_rownames("featurecode")

  colnames(scotSIMDinc) <- tolower(colnames(scotSIMDinc))

  create_table(h5filename = h5filename,
               component = "simd/income",
               df = scotSIMDinc,
               row_title = "datazones",
               row_names = rownames(scotSIMDinc),
               column_units = colnames(scotSIMDinc))
}
