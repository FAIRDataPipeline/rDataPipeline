#' Return all entries posted to a table in the data registry
#'
#' Get entries (from the data registry) in a particular table
#'
#' @param table a \code{string} specifying the name of the table
#' @param limit_results a \code{boolean} specifying whether or not to limit
#' the results, default is \code{TRUE}
#' @param detail a \code{string} specifying what level of detail to return;
#' options are \code{"all"} for all details or \code{"id"} for just URL and IDs
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a \code{data.frame} of entries in table, default is limited
#' to 100 entries
#'
#' @family get functions
#'
get_existing <- function(table,
                         limit_results = TRUE,
                         detail = "all",
                         endpoint = "http://localhost:8000/api/") {

  if (!check_table_exists(table))
    usethis::ui_stop(paste(
      usethis::ui_field(table),
      "is not a valid table - for available tables use get_tables()"))

  key <- get_token()
  h <- c(Authorization = paste("token", key))

  api_url <- paste0(endpoint, table)
  api_url <- file.path(dirname(api_url), basename(api_url), "")

  tryCatch({

    # Get the 100 newest results
    output <- httr::GET(api_url,
                        httr::add_headers(.headers = h)) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    results <- output$results

    if (!limit_results) {
      # Get the remaining results by using a while loop to update results
      # from all pages - because pagination is enabled, the next will only be
      # null if there are no more pages (note that next is a reserved word so
      # it must be wrapped in backticks)
      while (!is.null(output$`next`)) {
        tmp_output <- httr::GET(file.path(output$`next`)) %>%
          httr::content(as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
        results <- c(results, tmp_output$results)
        output <- tmp_output
      }
    }

    # Some tables contain lists, flatten them first or bind_rows() will error
    for (i in seq_along(results)) {
      for (ii in names(results[[i]])) {
        if (is.list(results[[i]][[ii]])) {
          if (!length(results[[i]][[ii]]))
            results[[i]][[ii]] <- NA
          else
            results[[i]][[ii]] <- paste0(unlist(results[[i]][[ii]]), ",",
                                         collapse = "")
          }
      }
    }
  }, error = function(e) {
    stop("an api error occured, please try again")
  })

  # Convert NULL values to NA to prevent rbind from erroring
  results <- lapply(results, function(x)
    sapply(x, function(y) if (is.null(y)) NA else y))

  # bind the results into a dataframe
  results <- dplyr::bind_rows(results)

  # select only url and id if detail is set to "id"
  if (detail == "id") {
    if (length(results) > 1)
      results <- results %>%
        dplyr::select(url) %>%
        dplyr::mutate(id = basename(url))
  }

  if (length(results) == 0)
    message("No results were returned")
  results

}
