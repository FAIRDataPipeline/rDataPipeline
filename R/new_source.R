#' new_source
#'
#' @param id
#' @param name
#' @param abbreviation
#' @param website
#'
#' @export
#'
new_source <- function(id,
                       name,
                       abbreviation,
                       website) {

  post_data(
    table = "source",
    data =  list(id = id,
                 name = name,
                 abbreviation = abbreviation,
                 website = website))
}
