#' new_source
#'
#' @param name e.g. "Scottish Government Open Data Repository"
#' @param abbreviation e.g. "Scottish Government Open Data Repository"
#' @param website e.g. "https://statistics.gov.scot/"
#' @param key key
#'
#' @export
#'
new_source <- function(name,
                       abbreviation,
                       website,
                       key) {

  post_data(
    table = "source",
    data =  list(name = name,
                 abbreviation = abbreviation,
                 website = website),
    key)
}
