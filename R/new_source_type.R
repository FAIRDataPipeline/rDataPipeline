#' new_source_type
#'
#' @param name name of the source
#' @param description free text description of the source
#'
#' @export
#'
new_source_type <- function(name,
                            description) {

  list(name = name,
       description = description)
}
