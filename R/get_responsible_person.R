#' get_responsible_person
#'
#' @param full_name
#'
#' @export
#'
get_responsible_person <- function(full_name) {

  tmp <- httr::GET(file.path(endpoint, "users", ""),
                   httr::add_headers(.headers = headers)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  ind <- lapply(tmp, function(x) x$full_name == full_name) %>%
    unlist() %>%
    which()
  tmp[[ind]]$url
}
