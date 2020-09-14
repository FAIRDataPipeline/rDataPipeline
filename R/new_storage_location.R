#' new_storage_location
#'
#' Upload information to the \code{storage_location} table in the data registry
#'
#' @param path a \code{string} specifying the path from the \code{storage_root}
#' URI to the item location, which when appended to \code{storage_root} URI
#' produces a complete URL
#' @param hash a \code{string} specifying the SHA1 hash of the file stored in
#' `storage_location`
#' @param storage_root_id a \code{string} specifying the API URL of the
#' associated \code{storage_root} table
#' @param key API token from data.scrc.uk
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' new_storage_location(path = "master/SCRC/human/infection/SARS-CoV-2/latent-period/0.1.0.toml",
#' hash = get_hash(file.path("local", "file", "location", "0.1.0.toml")),
#' storage_root_id = "https://data.scrc.uk/api/storage_root/14/",
#' key)
#' }}
#'
new_storage_location <- function(path,
                                 hash,
                                 storage_root_id,
                                 key) {

  if(grepl("^/", path)) stop(paste(path, "mustn't have a prepending slash"))

  post_data(table = "storage_location",
            data = list(path = path,
                        hash = hash,
                        storage_root = storage_root_id),
            key)
}
