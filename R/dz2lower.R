#' dz2lower
#'
#' @param population_dz
#' @param convert_to c("DZ", "UR", "IZ", "LA", "HB", "MMW", "SPC")
#' @export
#'
dz2lower <- function(population_dz, convert_to) {
  # Read in conversion table
  conversion_table <- read_excel("data-raw/SIMD+2020v2+-+datazone+lookup.xlsx",
                                 sheet = 3) %>%
    dplyr::rename(DZcode = DZ,
                  URcode = URclass) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  # Check that convert_to is valid
  assertthat::assert_that(convert_to %in% (colnames(conversion_table) %>%
                                             .[grepl("name$", .)] %>%
                                             gsub("name", "", .)))

  # Check that all population_dz datazones are in the lookup table
  assertthat::assert_that(all(population_dz$datazone %in%
                                conversion_table$DZcode))

  # Check that DZcode isn't duplicated in the conversion table
  assertthat::assert_that(!any(duplicated(conversion_subset$DZcode)))

  # Convert datazones
  regex <- paste0("DZ|", toupper(convert_to))
  columns <- colnames(conversion_table) %>% .[grepl(regex, .)]
  target <- paste0(convert_to, "code")

  subset_table <- conversion_table %>%
    dplyr::select(dplyr::contains(columns))

  output <- population_dz %>%
    dplyr::rename(DZcode = datazone) %>%
    dplyr::full_join(subset_table, by = "DZcode") %>%
    dplyr::select(-dplyr::contains(columns %>% .[-grep(target, .)])) %>%
    dplyr::group_by_at(vars(dplyr::contains(target))) %>%
    dplyr::summarise_all(~sum(.))
}
