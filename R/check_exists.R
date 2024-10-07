#' Check if entry exists in the data registry
#'
#' Check whether an entry already exists in a table (in the data registry)
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns \code{TRUE} if the entry exists and \code{FALSE} if it
#' doesn't
#'
check_exists <- function(table,
                         query) {

  output <- httr::GET(paste0("http://127.0.0.1:8000/api/", table, ""),
                      query = query)

  if (any(names(output) == "status_code")) {
    if (output$status_code == 404)
      stop("Table does not exist")
    else if (output$status_code == 200) {
      output <- output %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      if (any(names(output) == "detail")) {
        if (grepl("Invalid", output$detail))
          stop(output["detail"])
        }

      if (any(names(output) == "count"))
        return(ifelse(output$count > 0, TRUE, FALSE))
      }
    else
      stop(paste("error: status:", output$status_code, "returned"))
    }
  else
    stop("something went wrong")
}
