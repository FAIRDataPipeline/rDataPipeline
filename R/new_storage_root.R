#' new_storage_root
#'
#' @param name
#' @param root
#' @param accessibility
#'
#' @export
#'
new_storage_root <- function(name,
                             root,
                             accessibility) {

  post_data(table = "storage_root",
            data =  list(name = name,
                         root = root,
                         accessibility = accessibility),
            key)
}
