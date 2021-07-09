#' check_string
#'
#' @param table a \code{string} specifying the name of the table
#' @param this_field a \code{string} specifying the name of the field
#' @param this_query a \code{string} specifying the contents of the field
#'
check_string <- function(table,
                         this_field,
                         this_query) {

  if (is.character(this_query)) {
    return(TRUE)
  } else {
    usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                           "is incorrectly formatted"))
  }
}
