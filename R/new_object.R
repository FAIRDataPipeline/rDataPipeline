#' Post entry to object table
#'
#' Upload information to the \code{object} table in the data registry
#'
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object}
#' @param storage_location_url (optional) a \code{string} specifying the URL
#' of an entry in the \code{storage_location} table
#' @param authors_urls (optional) a \code{list} of URLs referencing
#' \code{author}s to associate with this \code{object}
#' @param file_type_url (optional) a \code{string} specifying the URL
#' of an entry in the \code{file_type} table
#'
#' @family new functions
#'
#' @export
#'
new_object <- function(description,
                       storage_location_url,
                       authors_urls = list(),
                       file_type_url) {

  data <- list()

  if (!missing(description))
    data$description <- description

  if (!missing(storage_location_url))
    data$storage_location <- storage_location_url

  if (!missing(authors_urls))
    data$authors <- authors_urls

  if (!missing(file_type_url))
    data$file_type <- file_type_url


  post_data(table = "object",
            data = data)
  # }
}
