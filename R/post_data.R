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

  result <- httr::POST(file.path("https://data.scrc.uk/api", table, ""),
                       body = jsonlite::toJSON(data, pretty = T,
                                               auto_unbox = T,
                                               force = T),
                       httr::content_type('application/json'),
                       httr::add_headers(.headers = h))

  tmp <- result %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(result$status == 201) {
    return(tmp$url)

  } else if(result$status == 409) {
    assertthat::assert_that(grepl("duplicate key value", tmp$detail) == T)
    tmp <- data[1]
    names(tmp) <- names(data[1])
    message(paste(table, "already exists"))
    return(get_url(table, tmp))

  } else {
    stop("Adding new data returned non-201 status code:", tmp)
  }

}
