#' check_field
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#' @param this_field a \code{string} specifying the name of the field
#' @param query_class a \code{string} specifying the class of the field
#' @param this_query a \code{string} specifying the contents of the field
#' @param method a \code{string} specifying the method, c("GET", "POST")
#' @param endpoint endpoint
#'
check_field <- function(table,
                        this_field,
                        query_class,
                        this_query,
                        method,
                        endpoint) {

  if (is.character(this_query)) {

    if (method == "POST") {
      if (grepl(endpoint, this_query)) {
        return(TRUE)
      } else {
        usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                               "is incorrectly formatted"))
      }

    } else if (method == "GET") {
      if (grepl("^[0-9]*$", this_query)) {
        return(TRUE)
      } else {
        usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                               "is incorrectly formatted"))
      }
    }

  } else {
    usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                           "is incorrectly formatted"))
  }
}
