#' Post entry to data registry
#'
#' Post data to registry
#'
#' @param table table name as a character
#' @param data data as a named list
#'
#' @export
#' @keywords internal
#'
post_data <- function(table, data) {

  key <- readLines(file.path("~", ".scrc", "TOKEN.txt"))
  h <- c(Authorization = paste("token", key))
  api_url <- paste0("http://localhost:8000/api", table, sep = "/")

  # Check there is a trailing slash (windows issue with file.path())
  api_url <- ifelse(substring(api_url, nchar(api_url)) == "/", api_url,
                    paste(api_url, "/", sep = ""))

  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to localhost port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  while (continue) {
    tryCatch({ # Try retrieving entry
      result <- httr::POST(api_url,
                           body = jsonlite::toJSON(data, pretty = T,
                                                   auto_unbox = T,
                                                   force = T),
                           httr::content_type('application/json'),
                           httr::add_headers(.headers = h))
      continue <- FALSE
    },
    error = function(e) {
    })
  }

  # Status 404: Not found (table doesn't exist)
  if(result$status == 404)
    usethis::ui_stop(paste(usethis::ui_field(gsub("_", " ", table)),
                           "does not exist"))

  tmp <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  # Status 201: created
  if(result$status == 201) {
    return(tmp$url)

    # Status 409: Conflict (entry already exists)
  } else if(result$status == 409) {

    tryCatch({
      return(get_url(table, clean_query(data))) },
      error = function(err) {
        usethis::ui_stop("Conflict with existing entry")
      })

    # Status non-201 (something went wrong)
  } else {
    usethis::ui_stop(tmp)
  }

}
