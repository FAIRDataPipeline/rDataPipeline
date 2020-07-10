#' check_exists
#'
#' @param table
#'
#' @export
#'
check_exists <- function(table, query) {

  suppressWarnings(
    tmp <- httr::GET(file.path("https://data.scrc.uk/api", table, ""),
                      query = query) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  )

  ifelse(length(output) == 0, return(FALSE), return(TRUE))
}
