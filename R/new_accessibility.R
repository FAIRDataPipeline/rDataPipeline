#' new_accessibility
#'
#' @param name Name of accessibility type
#' @param description Free text description of the accessibility type
#' @param access_info Type of accessibility
#' @param key GitHub key
#'
#' @export
#'
new_accessibility <- function(name,
                              description,
                              access_info,
                              key) {

  post_data(table = "accessibility",
            data =  list(name = name,
                         description = description,
                         access_info = access_info,
                         key = key),
            key)
}
