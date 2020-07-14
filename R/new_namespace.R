#' new_namespace
#'
#' @param name e.g.
#' @param key key
#'
#' @export
#'
new_namespace <- function(name,
                          key) {

  post_data(table = "namespace",
            data =  list(name = name),
            key)
}
