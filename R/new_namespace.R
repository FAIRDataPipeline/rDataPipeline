#' new_namespace
#'
#' @param name
#'
#' @export
#'
new_namespace <- function(name) {

  post_data(table = "namespace",
            data =  list(name = name),
            key)
}
