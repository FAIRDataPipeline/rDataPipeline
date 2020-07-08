#' new_prefix
#'
#' @param id
#' @param name
#'
#' @export
#'
new_prefix <- function(id,
                       name) {

  post_data(table = "Prefix",
            data =  list(id = id,
                         name = name))
}
