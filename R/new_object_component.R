#' Post entry to object_component table
#'
#' Upload information to the \code{object_component} table in the data registry
#' 
#' @keywords internal
#'
#' @param object_url a \code{string} specifying the URL of an existing
#' \code{object}
#' @param name a \code{string} specifying the name of the
#' \code{object_component}, unique in the context of \code{object_component}
#' and its \code{object} reference
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object_component}
#' @param whole_object a \code{boolean} flag specifying whether or not this
#' \code{object_component} refers to the whole object or not - default is
#' \code{FALSE}
#' @param issues_urls (optional) a \code{list} of \code{issues} URLs to
#' associate with this \code{object}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' Note that the \code{object_component} table contains \code{issues} as an
#' additional optional field. This is not included here. Instead use
#' \code{attach_issue()} and associated functionality to attach issues to
#' objects and object components.
#'
#' @family new functions
#'
new_object_component <- function(object_url,
                                 name,
                                 description,
                                 whole_object = FALSE,
                                 issues_urls,
                                 endpoint = "http://127.0.0.1:8000/api/") {

  data <- list(object = object_url,
               name = name,
               whole_object = whole_object)

  if (!missing(description))
    data$description <- description

  if (!missing(issues_urls))
    data$issues <- issues_urls

  post_data(table = "object_component",
            data = data,
            endpoint = endpoint)
}
