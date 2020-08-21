#' Get Queryable Fields
#'
#' @param table name of table
#'
#' @return a character vector of queryable fields
#'
#' @export
#'
get_table_queryable <- function(table)
{
  out <- httr::VERB("OPTIONS", file.path("https://data.scrc.uk/api", table, "")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  as.character(out$filter_fields)
}
