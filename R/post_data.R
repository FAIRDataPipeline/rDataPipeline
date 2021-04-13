#' Post entry to data registry
#'
#' Post data to registry
#'
#' @param table table name as a character
#' @param data data as a named list
#' @param ... internal parameters
#'
#' @export
#' @keywords internal
#'
post_data <- function(table, data, ...) {

  key <- readLines(file.path("~", ".scrc", "TOKEN.txt"))

  # If the skip_table_validation argument has been input, skip validate_table()
  # dots <- list(...)
  # if(any(names(dots) %in% "skip_table_validation"))
  #   skip_table_validation <- dots[["skip_table_validation"]] else
  #     skip_table_validation <- FALSE
  # if(!skip_table_validation)
  #   table <- validate_table(table, key)

  # Validate fields
  # data <- validate_fields(table, data, key)

  h <- c(Authorization = paste("token", key))

  api_url <- file.path("http://localhost:8000/api", table, "")

  # Check there is a trailing slash (windows issue with file.path())
  api_url <- ifelse(substring(api_url, nchar(api_url)) == "/", api_url,
                    paste(api_url, "/", sep = ""))

  result <- httr::POST(api_url,
                       body = jsonlite::toJSON(data, pretty = T,
                                               auto_unbox = T,
                                               force = T),
                       httr::content_type('application/json'),
                       httr::add_headers(.headers = h))

  table_name <- gsub("_", " ", table)

  if(result$status == 404)
    stop("Adding new data returned non-201 status code: (404) table does not exist")

  tmp <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(result$status == 201) {
    usethis::ui_done(paste("Added", usethis::ui_value(data$name), "to",
                           usethis::ui_value(table_name)))
    return(tmp$url)

  } else if(result$status == 409) {

    tryCatch(
      {
        usethis::ui_done(paste("Added", usethis::ui_value(data$name), "to",
                               usethis::ui_value(table_name)))
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
