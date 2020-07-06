#' get_responsible_person
#'
#' @param key
#'
#' @export
#'
get_responsible_person <- function(key) {

  h <- c(Authorization = paste("token", key))
  username <- system2("git", "config user.name", stdout = TRUE)

  tmp <- httr::GET(file.path("https://data.scrc.uk/api", "users", ""),
                   httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  ind <- lapply(tmp, function(x) x$username == username) %>%
    unlist() %>%
    which()

  tmp[[ind]]$url
}
