#' get_existing
#'
#' Returns all (or the first 100) entries from a particular table
#' in the data registry
#'
#' To return a vector of all tables in the data registry, run \code{get_tables()}
#'
#' @param table an \code{string} specifying a table in the data registry
#' @param limit_results a \code{boolean} specifying whether or not to limit the
#' results to 100 (default = \code{TRUE})
#' @param detail what level of detail to return; use \code{"all"} for all
#' details or \code{"id"} for just URL and IDs
#'
#' @return Returns a \code{data.frame} comprising all (or the first 100) entries
#' from a particular table in the data registry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' get_existing("storage_root")
#' }}
#'
get_existing <- function(table, limit_results = TRUE, detail = "all") {

  if(!check_table_exists(table))
    stop(paste0("Table: ", table, " does not exist\n",
                "For available tables use: get_tables()"))

  tryCatch({
    # Get the first 100 results
    output <- httr::GET(paste("http://data.scrc.uk/api", table, "", sep = "/")) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    results <- output$results

    if(!limit_results) {
      # Get the remaining results by using a while loop to update results
      # from all pages. Because pagination is enabled next will only be null
      # if there are no more pages. Note that next is a reserved word so
      # wrap it in ``.
      while(!is.null(output$`next`)){
        tmp_output <- httr::GET(file.path(output$`next`)) %>%
          httr::content(as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
        results <- c(results, tmp_output$results)
        output <- tmp_output
      }
    }

    # some tables contain lists, flatten them first or bind_rows() will error
    for(i in seq_along(results)){
      for(ii in names(results[[i]])){
        if(is.list(results[[i]][[ii]])){
          if(!length(results[[i]][[ii]]))
            results[[i]][[ii]] <- NA
          else
            results[[i]][[ii]] <- paste0(unlist(results[[i]][[ii]]), ",",
                                         collapse="")}
      }
    }
  }, error = function(e){
    stop("an api error occured, please try again")
  })

  # Convert NULL values to NA to prevent rbind from erroring
 results <- lapply(results, function(x)
   sapply(x, function(y) if(is.null(y)) NA else y))

  # bind the results into a dataframe
  results <- dplyr::bind_rows(results)

  # select only url and id if detail is set to "id"
  if(detail == "id"){
    if(length(results) > 1)
      results <- results %>% dplyr::select(url) %>% dplyr::mutate(id = basename(url))
  }

  if(length(results) == 0)
    message("No results were returned")
  results

}
