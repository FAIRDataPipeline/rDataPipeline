#' post_data
#'
#' Post data to registry
#'
#' @param table table
#' @param data data
#' @param key key
#'
#' @export
#'
post_data <- function(table,
                      data,
                      key) {

  h <- c(Authorization = paste("token", key))

  api_url <- file.path("https://data.scrc.uk/api", table, "")

  # Check there is a trailing slash (windows issue with file.path())
  api_url <- ifelse(substring(api_url, nchar(api_url)) == "/", api_url,
                    paste(api_url, "/", sep = ""))

  result <- httr::POST(api_url,
                       body = jsonlite::toJSON(data, pretty = T,
                                               auto_unbox = T,
                                               force = T),
                       httr::content_type('application/json'),
                       httr::add_headers(.headers = h))

  if(result$status == 404)
    stop("Adding new data returned non-201 status code: (404) table does not exist")

  tmp <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(result$status == 201) {
    return(tmp$url)

  } else if(result$status == 409) {

    tmp <- result %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)

    message(paste(table, "already exists:", tmp$detail))

    return(get_url(table, clean_query(data)))

  } else {
    stop("Adding new data returned non-201 status code: (", result$status , ") ", tmp)
  }

}
