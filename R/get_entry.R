#' get_entry
#'
#' @param table table must exist
#' @param query query a \code{list} of fields and values to query
#' if no query is provided it will return the last entry
#'
#' @export
#'
get_entry <- function(table, query = list()) {
  # to be Deprecated
  if(is.character(query)){
    if(query == ""){
      warning("using query as \"\" is deprecated and will be removed")
      query <- list()
    }
  }

  if(!check_table_exists(table))
    stop(paste("table ", table, " does not exist"))
  if(!check_query(table, query))
    stop("not a valid query for table")

   out <- httr::GET(file.path("https://data.scrc.uk/api", table, ""),
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
