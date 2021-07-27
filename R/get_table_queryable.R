#' Get queryable fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a character vector of queryable fields
#' @keywords internal
#'
get_table_queryable <- function(table, endpoint) {

  api_url <- paste0(endpoint, table)
  api_url <- file.path(dirname(api_url), basename(api_url), "")

  out <- httr::VERB("OPTIONS", api_url) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  as.character(out$filter_fields)
}
