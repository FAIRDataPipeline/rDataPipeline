#' get_entry
#'
#' @param table table must exist
#' @param query a \code{list} of fields and values to query
#'
#' @export
#'
get_entry <- function(table, query) {

  if(!is.list(query)) stop("query must be a list")
  if(length(query) == 0) stop("query is empty")

  if(!check_table_exists(table))
    stop(paste("table ", table, " does not exist"))
  if(!check_query(table, query))
    stop("not a valid query for table")

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
