#' Post entry to code_repo_release table
#'
#' Upload information to the \code{code_repo_release} table in the data registry
#'
#' @param name a \code{string} specifying the name of an official release of
#' code
#' @param version a \code{string} specifying the version release
#' (conforming with semantic versioning syntax)
#' @param website (optional) a \code{string} specifying the URL of the
#' website for this code release
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_code_repo_release <- function(name,
                                  version,
                                  object_url,
                                  website,
                                  endpoint = "http://localhost:8000/api/") {

  data <- list(name = name,
               version = version,
               object = object_url)

  if (!missing(website))
    data$website <- website

  post_data(table = "code_repo_release",
            data = data,
            endpoint = endpoint)
}
