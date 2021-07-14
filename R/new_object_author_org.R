#' Post entry to object_author_org table
#'
#' Upload information to the \code{object_author_org} table in the data registry
#'
#' @param object_url a \code{string} specifying the URL of an existing
#' \code{object}
#' @param author_url a \code{string} specifying the URL of an existing
#' \code{author}
#' @param organisations_urls (optional) a \code{string} specifying the URL of an
#' existing \code{organisation} to associate with this \code{object_author_org}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
#' @export
#'
new_object_author_org <- function(object_url,
                                  author_url,
                                  organisations_urls = list(),
                                  endpoint = "http://localhost:8000/api/") {

  data <- list(object = object_url,
               author = author_url)

  if (!missing(organisations_urls))
    data$organisations <- organisations_urls

  post_data(table = "object_author_org",
            data = data,
            endpoint = endpoint)
}
