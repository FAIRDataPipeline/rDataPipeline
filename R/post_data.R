#' post_data
#'
#' @param table
#' @param data
#' @param headers
#'
#' @export
#'
post_data <- function(table,
                      data,
                      headers) {

  result <- httr::POST(file.path("http://data.scrc.uk/api", table, ""),
                       body =  jsonlite::toJSON(data, pretty = T, auto_unbox = T),
                       httr::content_type('application/json'),
                       httr::add_headers(.headers = headers),
                       verbose())

  if(results$status == 200)
    print("Data added to", table) else
      stop("Adding new data returned non-200 status code:", result)
}
