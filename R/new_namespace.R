#' Post to namespace table
#'
#' Upload information to the \code{namespace} table in the data registry
#'
#' @param name a \code{string} specifying the name of the namespace *e.g.* "SCRC"
#' @param key API token from data.scrc.uk
#'
#' @family new functions
#'
#' @export
#'
new_namespace <- function(name,
                          key) {

  post_data(table = "namespace",
            data =  list(name = name),
            key)
}
