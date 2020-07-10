#' get_existing
#'
#' @param table
#'
#' @export
#'
get_existing <- function(table) {

  suppressWarnings(
    output <- httr::GET(file.path("http://data.scrc.uk/api", table, "")) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      dplyr::bind_rows()
  )

  if(length(output) == 0)
    print("Returned no entries") else output

}
