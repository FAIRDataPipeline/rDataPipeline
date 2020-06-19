#' post_data
#'
#' @param endpoint api endpoint
#' @param table
#' @param data
#'
#' @export
#'
post_data <- function(endpoint = "http://data.scrc.uk/api",
                      table,
                      data) {

  result <- httr::POST(file.path(endpoint, table),
                       body =  jsonlite::toJSON(data, pretty = T, auto_unbox = T),
                       httr::add_headers(headers),
                       httr::content_type('application/json'),
                       # encode = "json",
                       verbose())

  if(results$status == 200)
    print("Data added to", table) else
      stop("Adding new data returned non-200 status code:", result)
}
