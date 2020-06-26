#' new_processing_script
#'
#' @param name Name of the processing script
#' @param responsible_person Name of the responsible person
#' @param store Name of the storage_location, e.g. "Model File"
#' @param versions
#' @param key GitHub key
#'
#' @export
#'
new_processing_script <- function(name,
                                  responsible_person,
                                  store,
                                  versions,
                                  key) {

  rp_url <- get_responsible_person(responsible_person, key)
  location_url <- get_url("storage_location", list(name = store))

  post_data(table = "processing_script",
            data = list(name = name,
                        responsible_person = rp_url,
                        store = location_url,
                        versions = versions),
            key)
}
