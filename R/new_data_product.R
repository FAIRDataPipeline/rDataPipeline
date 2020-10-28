#' Post to data_product table
#'
#' Upload information to the \code{data_product} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{data_product}
#'  *e.g.* "records/SARS-CoV-2/scotland/human-mortality"
#' @param version a \code{string} specifying the version identifier of the
#' \code{data_product} (must conform to semantic versioning syntax) *e.g.* "0.1.0"
#' @param object_id a \code{string} specifying the API URL of the
#' associated \code{object} table *e.g.* "https://data.scrc.uk/api/object/156/",
#' @param namespace_id a \code{string} specifying the API URL of the
#' associated \code{namespace} table *e.g.* "https://data.scrc.uk/api/namespace/2/"
#' @param key API token from data.scrc.uk
#' @export
#'
new_data_product <- function(name,
                             version,
                             object_id,
                             namespace_id,
                             key) {

  post_data(table = "data_product",
            data =  list(name = name,
                         version = version,
                         object = object_id,
                         namespace = namespace_id),
            key)
}
