#' new_source
#'
#' @param name
#' @param abbreviation
#' @param website
#'
#' @export
#'
new_source <- function(name,
                       abbreviation,
                       website) {

  post_data(
    table = "source",
    data =  list(name = name,
                 abbreviation = abbreviation,
                 website = website),
    key)
}
