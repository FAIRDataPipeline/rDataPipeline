#' get_existing
#'
#' @param table
#' @param full
#'
#' @export
#'
get_existing <- function(table, full = F) {

  suppressWarnings(
    output <- httr::GET(file.path("http://data.scrc.uk/api", table, "")) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      data.table::rbindlist(use.names = TRUE, fill = TRUE)
  )

  if(length(output) == 0) {
    print("Returned no entries")

  } else if(full) {
    return(output)

  } else {
    if(table == "data_product_type") {
      return(output %>% dplyr::select(name, description))
    } else if (table == "storage_type") {
      return(output %>% dplyr::select(name, description))
    } else if (table == "storage_root") {
      return(output %>% dplyr::select(name, description, uri, type))
    } else if (table == "storage_location") {
      return(output %>% dplyr::select(name, description, path, hash,
                                      local_cache_url, responsible_person,
                                      store_root))
    } else if (table == "accessibility") {
      return(output %>% dplyr::select(name, description, access_info))
    } else if (table == "source_type") {
      return(output %>% dplyr::select(name, description))
    } else if (table == "source") {
      return(output %>% dplyr::select(name, description, responsible_person,
                                      store, source_type))
    } else if (table == "source_version") {
      return(output %>% dplyr::select(version_identifier, description,
                                      responsible_person, supercedes, source,
                                      store, accessibility))
    } else if (table == "data_product") {
      return(output %>% dplyr::select(name, description, responsible_person,
                                      type, versions))
    } else if (table == "processing_script") {
      return(output %>% dplyr::select(name, responsible_person, store,
                                      versions))
    } else if (table == "processing_script_version") {
      return(output %>% dplyr::select(version_identifier, responsible_person,
                                      supercedes, processing_script, store,
                                      accessibility, data_product_versions))
    } else if (table == "data_product_version") {
      return(output %>% dplyr::select(version_identifier, description,
                                      responsible_person, supercedes,
                                      data_product, store, accessibility,
                                      processing_script_version,
                                      source_versions, components, model_runs))
    } else if (table == "data_product_version_component") {
      return(output %>% dplyr::select(name, responsible_person,
                                      data_product_version, model_runs))
    } else if (table == "model") {
      return(output %>% dplyr::select(name, description, responsible_person,
                                      store, versions))
    } else if (table == "model_version") {
      return(output %>% dplyr::select(version_identifier, description,
                                      responsible_person, supercedes, model,
                                      store, accessibility, model_runs))
    } else if (table == "model_run") {
      return(output %>% dplyr::select(release_date, description, model_config,
                                      submission_script, responsible_person,
                                      model_version, supercedes, inputs,
                                      outputs))
    } else {
      stop("Unknown", table)
    }
  }

}
