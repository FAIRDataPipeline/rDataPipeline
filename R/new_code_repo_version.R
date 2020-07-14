#' new_code_repo_release
#'
#' @param name e.g.
#' @param version e.g.
#' @param website e.g.
#' @param object e.g.
#' @param key key
#'
#' @export
#'
new_code_repo_release <- function(name,
                                  version,
                                  website,
                                  object,
                                  key) {

  object_url <- get_url("object", list(id = object))

  post_data(table = "code_repo_release",
            data = list(name = name,
                        version = version,
                        website = website,
                        object = object_url),
            key)
}
