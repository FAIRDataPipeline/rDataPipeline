#' Post entry to user_author_org table
#'
#' Upload information to the \code{user_author_org} table in the data registry
#'
#' @param user_url a \code{string} specifying the URL of an existing
#' \code{user}
#' @param author_url a \code{string} specifying the URL of an existing
#' \code{author}
#' @param organisations_urls (optional) a \code{list} of URLs specifying which
#' \code{organisation}s to associate with this \code{user_author_org}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
#' @export
#'
new_user_author_org <- function(user_url,
                                author_url,
                                organisations_urls = list(),
                                endpoint = "http://localhost:8000/api/") {

  data <- list(user = user_url,
               author = author_url)

  if (!missing(organisations_urls))
    data$organisations <- organisations_urls

  post_data(table = "user_author_org",
            data =  data,
            endpoint = endpoint)
}
