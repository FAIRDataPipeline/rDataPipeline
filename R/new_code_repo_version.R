#' new_code_repo_version
#'
#' @param object_id
#' @param name
#' @param version
#' @param website
#'
#' @export
#'
new_code_repo_version <- function(object_id,
                                  name,
                                  version,
                                  website) {

  object_url <- get_url("Object", list(id = object_id))

  post_data(table = "CodeRepoVersion",
            data = list(object_id = object_url,
                        name = name,
                        version = version,
                        website = website))
}
