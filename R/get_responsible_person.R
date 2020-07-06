#' get_responsible_person
#'
#' @param full_name
#' @param key
#'
#' @export
#'
get_responsible_person <- function(full_name, key) {

  h <- c(Authorization = paste("token", key))

  tmp <- httr::GET(file.path("https://data.scrc.uk/api", "users", ""),
                   httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  ind <- lapply(tmp, function(x) x$full_name == full_name) %>%
    unlist() %>%
    which()

  tmp[[ind]]$url
}
