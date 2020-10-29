#' Convert census geographies to lower resolution
#'
#' @param dat data
#' @param convert_to c("dz", "ur", "iz", "la", "hb", "mmw", "spc")
#' @param conversion_table conversion table
#'
#' @export
#'
convert_to_lower <- function(dat, convert_to, conversion_table) {

  # Convert datazones
  regex <- paste0("AREA|", toupper(convert_to))
  columns <- colnames(conversion_table)
  columns <- columns[grepl(regex, columns)]
  target.code <- paste0(toupper(convert_to), "code")
  target.name <- paste0(toupper(convert_to), "name")

  subset.table <- conversion_table %>%
    dplyr::select(dplyr::contains(columns)) %>%
    unique()

  output <- dat %>%
    dplyr::full_join(subset.table, by = "AREAcode") %>%
    dplyr::select(-dplyr::contains(columns), target.code)
  output <- dplyr::group_by_at(output,vars(dplyr::contains(target.code))) %>%
    dplyr::summarise_all(~sum(.)) %>%
    dplyr::arrange_at(vars(dplyr::contains(target.code)))

  reference <- subset.table %>%
    dplyr::select_at(vars(one_of(target.code, target.name))) %>%
    unique() %>%
    dplyr::arrange_at(vars(dplyr::contains(target.code)))

  assertthat::assert_that(all(output[, target.code] ==
                                reference[, target.code]))

  list(data = output,
       area.names = reference[, c(target.code, target.name)])
}










