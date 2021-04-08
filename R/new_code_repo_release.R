#' Post entry to code_repo_release table
#'
#' Upload information to the \code{code_repo_release} table in the data registry
#'
#' @param name a \code{string} specifying the name of an official release of
#' code *e.g.* "ScottishCovidResponse/SCRCdata"
#' @param version a \code{string} specifying the version release
#' (conforming with semantic versioning syntax) *e.g.* "0.1.0"
#' @param website (optional) a \code{string} specifying the URL of the
#' website for this code release
#' *e.g.* "https://github.com/ScottishCovidResponse/SCRCdata"
#' @param object_id a \code{string} specifying the API URL of the associated
#' `object` table *e.g.* "https://data.scrc.uk/api/object/154/"
#'
#' @family new functions
#'
#' @export
#'
new_code_repo_release <- function(name,
                                  version,
                                  website = "",
                                  object_id) {

  post_data(table = "code_repo_release",
            data = list(name = name,
                        version = version,
                        website = website,
                        object = object_id))
}
