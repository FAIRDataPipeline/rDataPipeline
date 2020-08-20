#' get_existing
#'
#' @param table table
#' @param limit_results boolean whether or not to limit
#' the results default \code{TRUE}
#' @param detail what level of detail to return
#' use \code{"all"} for all details or \code{"id"} for just URL and ID's
#' @return returns a data.frame of entries in table
#' default is limited to 100 entries
#' @export
#'
get_existing <- function(table, limit_results = TRUE, detail = "all") {

  if(!check_table_exists(table))
    stop(paste0("Table: ", table, " does not exist\n",
                "For available tables use: get_tables()"))

  tryCatch({

    # Get the first 100 results
    output <- httr::GET(file.path("http://data.scrc.uk/api", table, "")) %>%
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
