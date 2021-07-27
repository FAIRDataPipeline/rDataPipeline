#' Post entry to namespace table
#'
#' Upload information to the \code{namespace} table in the data registry
#'
#' @param name a \code{string} specifying the name of the namespace
#' @param full_name (optional) a \code{string} specifying the full name of the
#' namespace
#' @param website (optional) a \code{string} specifying the website URL
#' associated with the namespace
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_namespace <- function(name,
                          full_name,
                          website,
                          endpoint = "http://localhost:8000/api/") {

  data <- list(name = name)

  if (!missing(full_name))
    data$full_name <- full_name

  if (!missing(website))
    data$website <- website

  post_data(table = "namespace",
            data = data,
            endpoint = endpoint)
}
