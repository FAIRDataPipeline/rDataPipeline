#' Post to source table
#'
#' Upload information to the \code{source} table in the data registry
#'
#' @param name a \code{string} specifying the name of the source
#' *e.g.* "Scottish Government Open Data Repository"
#' @param abbreviation a \code{string} specifying the common abbreviation
#' of the source (if available) *e.g.* "Scottish Government Open Data Repository"
#' @param website (optional) a \code{string} specifying the website URL
#' associated with the data source *e.g.* "https://statistics.gov.scot/"
#' @param key API token from data.scrc.uk
#'
#' @export
#'
new_source <- function(name,
                       abbreviation,
                       website = "",
                       key) {

  post_data(table = "source",
            data =  list(name = name,
                         abbreviation = abbreviation,
                         website = website),
            key)
}
