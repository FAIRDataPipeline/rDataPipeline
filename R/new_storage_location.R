#' Post entry to storage_location table
#'
#' Upload information to the \code{storage_location} table in the data registry
#'
#' @param path a \code{string} specifying the path from the \code{storage_root}
#' URI to the item location, which when appended to \code{storage_root} URI
#' produces a complete URL
#' *e.g.* "master/SCRC/human/infection/SARS-CoV-2/latent-period/0.1.0.toml"
#' @param hash a \code{string} specifying the SHA1 hash of the file stored in
#' `storage_location` *e.g.* "7c0e14caec08674d7d4e52c305cb4320babaf90f"
#' @param storage_root_id a \code{string} specifying the API URL of the
#' associated \code{storage_root} table
#' *e.g.* "https://data.scrc.uk/api/storage_root/14/"
#'
#' @family new functions
#'
#' @export
#'
new_storage_location <- function(path,
                                 hash,
                                 storage_root_id) {

  post_data(table = "storage_location",
            data = list(path = path,
                        hash = hash,
                        storage_root = storage_root_id))
}
