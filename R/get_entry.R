#' Return all fields associated with a table entry in the data registry
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a \code{list} of fields present in the specified entry
#'
#' @export
#' @family get functions
#'
get_entry <- function(table, query, endpoint = "http://127.0.0.1:8000/api/") {

  key <- get_token()
  h <- c(Authorization = paste("token", key))

  # Can't get an empty entry
  if (is.list(query) && length(query) == 0)
    stop("a query must be defined")

  is_queryable(table = table,
               query = query,
               method = "GET",
               endpoint = endpoint)

  api_url <- paste0(endpoint, table)
  api_url <- file.path(dirname(api_url), basename(api_url), "")

  output <- httr::GET(api_url, query = query,
                      httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if (output$count == 0) {
    return(NULL)

  } else {
    return(output$results)
  }
}
