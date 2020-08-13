#' new_storage_root
#'
#' Upload information to the `storage_root` table in the data registry
#'
#' @param name a `string` specifying the name of the `storage_root`
#' *e.g.* "boydorr"
#' @param root a `string` specifying the URI to the root of a
#' `storage_location`, which is then prepended to a `storage_location`
#' *e.g.* "ftp://boydorr.gla.ac.uk/scrc/"
#' @param accessibility (optional) an `integer` value for the accessibility
#' enum, where 0 is public (default) and 1 is private
#' @param key API token from data.scrc.uk
#'
#' @export
#'
new_storage_root <- function(name,
                             root,
                             accessibility = "",
                             key) {

  post_data(table = "storage_root",
            data = list(name = name,
                        root = root,
                        accessibility = accessibility),
            key)
}
