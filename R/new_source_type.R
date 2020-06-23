#' new_source_type
#'
#' @param name Name of the source type
#' @param description Free text description of the source
#' @param key GitHub key
#'
#' @export
#'
new_source_type <- function(name,
                            description,
                            key) {

  post_data(table = "source_type",
            data = list(name = name,
                        description = description),
            key)
}
