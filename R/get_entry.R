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
#' \dontrun{
#' # Get list of entries
#' data_product <- "records/SARS-CoV-2/scotland/cases-and-management/ambulance"
#' get_entry("storage_location",
#' list(path = "/Users/SoniaM/datastore/config/20210422-115831.yaml",
#' hash = "e3caf6a4674bde77e6bd3cbc85b6a867adf7397b",
#' storage_root = "7"))
#' }
#'
get_entry <- function(table, query) {
  # Can't get an empty entry
  if(is.list(query) && length(query) == 0)
    stop("a query must be defined")
  # An empty string is not a valid query
  if(is.character(query) && query == "") stop("not a valid query")

  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to localhost port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  while (continue) {
    tryCatch({ # Try retrieving entry
      output <- httr::GET(paste0("http://localhost:8000/api/", table, ""),
                          query = query) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      continue <- FALSE
    },
    error = function(e) {
    })
  }

  if(output$count == 0) {
    message("Entry doesn't exist")
    return(NULL)

  } else {
    return(output$results)
  }
}
