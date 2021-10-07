#' check_datetime
#'
#' @param table a \code{string} specifying the name of the table
#' @param this_field a \code{string} specifying the name of the field
#' @param query_class a \code{string} specifying the class of the field
#' @param this_query a \code{string} specifying the contents of the field
#'
check_datetime <- function(table,
                           this_field,
                           query_class,
                           this_query) {

  if (any(query_class %in% "POSIXct")) {
    return(TRUE)

  } else if (is.character(this_query)) {
    is_date <- tryCatch({
      as.Date(this_query)
    },
    error = function(e) {
      NULL
    })

    if (is.na(is_date)) {
      usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                             "is incorrectly formatted"))

    } else if (!is.null(is_date)) {
      return(TRUE)

    } else {
      usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                             "is incorrectly formatted"))
    }

  } else {
    usethis::ui_stop(paste(table, "field", usethis::ui_field(this_field),
                           "is incorrectly formatted"))
  }

}
