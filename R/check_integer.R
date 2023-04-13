#' check_integer
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#' @param this_field a \code{string} specifying the name of the field
#' @param query_class a \code{string} specifying the class of the field
#' @param this_query a \code{string} specifying the contents of the field
#'
check_integer <- function(table,
                          this_field,
                          query_class,
                          this_query) {

  test <- (query_class == "numeric") &
    (this_query - round(this_query) < 0.0000001)

  if (test) {
    return(TRUE)
  } else {
    usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                           "is incorrectly formatted"))
  }
}
