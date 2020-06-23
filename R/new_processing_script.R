#' new_processing_script
#'
#' @param name
#' @param responsible_person
#' @param store
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
  location_url <- get_url(table = "storage_location", key = store)

  post_data(table = "processing_script",
            data = list(name = name,
                        responsible_person = rp_url,
                        store = location_url,
                        versions = versions),
            key)

}
