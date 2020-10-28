#' get_entry
#'
#' @param table table must exist
#' @param query a \code{list} of fields and values to query if no query
#' is provided it will return the last entry
#'
#' @export
#'
#' @examples
#' get_entry("storage_root", list(name = "github"))
#'
get_entry <- function(table, query = list()) {
  # An empty string is an invalid query
  if(is.character(query) && query == "") stop("not a valid query")
  # Check whether table is valid
  if(!check_table_exists(table)) stop(paste("table doesn't exist"))
  # Check whether query is valid (for table)
  if(!check_query(table, query)) stop("not a valid query for table")

  out <- httr::GET(paste("https://data.scrc.uk/api", table, "", sep = "/"),
                   query = query) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(out$count == 0) {
    message("Entry doesn't exist")
    return(NULL)

  } else {
    return(out$results)
  }
}
