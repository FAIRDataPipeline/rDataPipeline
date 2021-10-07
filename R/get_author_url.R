#' get_author_url
#'
#' @param endpoint a \code{string} specifying the registry endpoint
#'
get_author_url <- function(endpoint) {
  user_url <- get_url(table = "users",
                      query = list(username = "admin"),
                      endpoint = endpoint)
  assertthat::assert_that(length(user_url) == 1)
  user_id <- extract_id(user_url, endpoint = endpoint)
  user_author_url <- get_entry("user_author",
                               query = list(user = user_id),
                               endpoint = endpoint)
  assertthat::assert_that(length(user_author_url) == 1)
  user_author_url[[1]]$author
}
