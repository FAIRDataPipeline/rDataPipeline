#' Post entry to data registry
#'
#' Post data to registry
#'
#' @param table table name as a character
#' @param data data as a named list
#' @param key API token
#' @param ... internal parameters
#'
#' @export
#'
post_data <- function(table, data, key, ...) {

  key <- validate_token(key)

  # If the skip_table_validation argument has been input, skip validate_table()
  dots <- list(...)
  if(any(names(dots) %in% "skip_table_validation"))
    skip_table_validation <- dots[["skip_table_validation"]] else
      skip_table_validation <- FALSE
  if(!skip_table_validation)
    table <- validate_table(table, key)

  # Validate fields
  data <- validate_fields(table, data, key)

  # data <- validate_post_data(table, data, key)
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

    tryCatch(
      {
        message(paste(table, "already exists"))
        return(get_url(table, clean_query(data)))
      },
      error = function(err) {
        stop("fields don't match existing entry, so no URI was returned")
      }
    )

  } else {
    stop("Adding new data returned non-201 status code: (",
         result$status , ") ", tmp)
  }

}
