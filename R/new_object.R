#' Post entry to object table
#'
#' Upload information to the \code{object} table in the data registry
#'
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object}
#' @param storage_location_url (optional) a \code{string} specifying the URL
#' of an entry in the \code{storage_location} table
#' @param issues_urls (optional) a \code{list} of \code{issues} URLs to associate
#' with this \code{object}
#' @param authors_urls (optional) a \code{list} of \code{author} URLs to associate
#' with this \code{object}
#'
#' @family new functions
#'
#' @export
#'
new_object <- function(description,
                       storage_location_url,
                       issues_urls = list(),
                       authors_urls = list()) {

  data <- list(issues = issues_urls,
               authors = authors_urls)

  if (!missing(description))
    data$description <- description

  if (!missing(storage_location_url))
    data$storage_location <- storage_location_url

  post_data(table = "object",
            data = data)
  # }
}
