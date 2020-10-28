#' Get Queryable Fields
#'
#' @param table a \code{string} specifying the name of the table
#'
#' @return Returns a character vector of queryable fields
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' get_table_queryable("data_product")
#'
get_table_queryable <- function(table) {
  out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "",
                                     sep = "/")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  as.character(out$filter_fields)
}
