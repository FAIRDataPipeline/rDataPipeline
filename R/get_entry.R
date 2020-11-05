#' Return all fields associated with a table entry in the data registry
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns a \code{list} of fields present in the specified entry
#'
#' @family get functions
#'
#' @export
#'
#' @examples
#' # Get list of entries
#' data_product <- "records/SARS-CoV-2/scotland/cases-and-management/ambulance"
#' get_entry("data_product", list(name = data_product))
#'
get_entry <- function(table, query) {
  # Can't get an empty entry
  if(is.list(query) && length(query) == 0)
    stop("a query must be defined")
  # An empty string is not a valid query
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
