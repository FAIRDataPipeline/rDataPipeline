#' convert_url
#'
#' @export
#'
convert_url <- function(url, key) {
  h <- c(Authorization = paste("token", key))

  output <- GET(url = url,
                httr::add_headers(.headers = h)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if("name" %in% names(output)) {
    return(output$name)

  } else if("full_name" %in% names(output)) {
    return(output$full_name)
  } else if("version_identifier" %in% names(output)) {
    return(output$version_identifier)
  } else
    stop("error")
}
