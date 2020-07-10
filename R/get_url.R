#' get_url
#'
#' @param table
#' @param query
#'
#' @export
#'
get_url <- function(table, query = list()) {

  # tmp <- httr::GET(file.path("https://data.scrc.uk/api", table, ""),
  #                  query = query) %>%
  #   httr::content(as = "text", encoding = "UTF-8") %>%
  #   jsonlite::fromJSON(simplifyVector = FALSE)
  #
  # if(length(tmp) > 1) {
  #   stop("More than 1 object was returned")
  # } else if(length(tmp) == 0) {
  #   stop("No objects were returned")
  # } else {
  #   return(tmp[[1]]$url)
  # }
  tmp <- get_existing(table)
  tmp$url[which(tmp[,names(query)] == unname(query))]
}
