#' new_code_repo_release
#'
#' @param name e.g.
#' @param version e.g.
#' @param website e.g.
#' @param object_id e.g.
#' @param key key
#'
#' @export
#'
new_code_repo_release <- function(name,
                                  version,
                                  website,
                                  object_id,
                                  key) {

  post_data(table = "code_repo_release",
            data = list(name = name,
                        version = version,
                        website = website,
                        object = object_id),
            key)
}
