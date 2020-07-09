#' new_code_repo_release
#'
#' @param name
#' @param version
#' @param website
#' @param object
#'
#' @export
#'
new_code_repo_release <- function(name,
                                  version,
                                  website,
                                  object) {

  object_url <- get_url("object", list(id = object))

  post_data(table = "code_repo_release",
            data = list(name = name,
                        version = version,
                        website = website,
                        object = object_url),
            key)
}
