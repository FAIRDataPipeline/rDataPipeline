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
  api_url <- ifelse(substring(api_url, nchar(api_url)) == "/", api_url, paste(api_url, "/", sep = ""))

  result <- httr::POST(api_url,
                       body = jsonlite::toJSON(data, pretty = T,
                                               auto_unbox = T,
                                               force = T),
                       httr::content_type('application/json'),
                       httr::add_headers(.headers = h))

  if(result$status == 404)
    stop("Error: Is it possible the table does not exist?")


  tmp <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(result$status == 201) {
    return(tmp$url)

  } else if(result$status == 409) {
    assertthat::assert_that(grepl("duplicate key value", tmp$detail) == T)

    message(paste(table, "already exists"))
    return(get_url(table, clean_query(data)))

  } else if(result$status == 400) {
    if(table == "object") {
      message(paste(table, "already exists"))
      return(get_url("object", list(storage_location = clean_query(data))))

    } else if(table == "code_repo_release") {
      message(paste(table, "already exists"))
      return(get_url("code_repo_release", clean_query(data)))

    } else {
      stop("Adding new data returned non-201 status code: (", result$status , ") ", tmp)
    }

  } else {
    stop("Adding new data returned non-201 status code: (", result$status , ") ", tmp)
  }

}
