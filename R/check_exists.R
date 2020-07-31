#' check_exists
#'
#' @param table table name e.g.
#' @param query query valid for table
#'
#' @export
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
