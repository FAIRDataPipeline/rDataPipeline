#' new_source
#'
#' @param name
#' @param abbreviation
#' @param website
#' @param key
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
