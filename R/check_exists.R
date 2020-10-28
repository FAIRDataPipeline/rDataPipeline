#' check_exists
#'
#' Check whether an entry already exists in a table (in the data registry)
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns \code{TRUE} if the entry exists and \code{FALSE} if it
#' doesn't
#'
#' @export
#'
#' @examples
#' # Check whether "github" is in the storage_root table
#' check_exists("storage_root", list(name = "github"))
#'
#' # Check whether "not_a_root" is in the storage_root table
#' check_exists("storage_root", list(name = "not_a_root"))
#'
check_exists <- function(table,
                         query) {

    output <- httr::GET(file.path("https://data.scrc.uk/api", table, ""), query = query)

    if(any(names(output) == "status_code")){
      if(output$status_code == 404)
        stop("an error occured: does the table exist?")
      else if(output$status_code == 200){
      output <- output %>% httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)

      if(any(names(output) == "detail")){
       if(grepl("Invalid", output$detail))
         stop(output["detail"])}

      if(any(names(output) == "count"))
        return(ifelse(output$count > 0, TRUE, FALSE))}
      else
        stop(paste("error: status:", output$status_code, "returned"))}
    else
      stop("something went wrong")
}
