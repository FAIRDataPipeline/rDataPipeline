#' Post entry to data registry
#'
#' Post data to registry
#'
#' @param table table name as a character
#' @param data data as a named list
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @keywords internal
#'
post_data <- function(table, data, endpoint) {

  key <- get_token()
  h <- c(Authorization = paste("token", key),
         Accept = 'application/json; version=1.0.0')

  api_url <- paste0(endpoint, table)
  api_url <- file.path(dirname(api_url), basename(api_url), "")
  if(substring(api_url,nchar(api_url)) != "/"){
    api_url <- paste0(api_url, "/")
  }

  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to 127.0.0.1 port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  while (continue) {
    result <- httr::POST(api_url,
                         body = jsonlite::toJSON(data, pretty = T,
                                                 auto_unbox = T,
                                                 force = T),
                         httr::content_type("application/json"),
                         httr::add_headers(.headers = h))
    continue <- FALSE
  }

  # Status 404: Not Found (table doesn't exist)
  if (result$status == 404)
    usethis::ui_stop(paste(usethis::ui_field(gsub("_", " ", table)),
                           "does not exist"))

  detail <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  # Status 201: Created
  if (result$status == 201) {
    return(detail$url)

    # Status 400: Bad Request (error in field)
  } else if (result$status == 400) {
    stop(paste(names(detail), unlist(detail)))

    # Status 409: Conflict (entry already exists)
  } else if (result$status == 409) {
    new_query <- clean_query(data = data,
                             endpoint = endpoint)
    output <- get_entry(table = table,
                        query = new_query,
                        endpoint = endpoint)
    if (is.null(output)) stop(detail)
    assertthat::assert_that(length(output) == 1)
    return(output[[1]]$url)

    # Status non-201 (something went wrong)
  } else {
    stop(detail)
  }

}
