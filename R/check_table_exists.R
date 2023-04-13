#' Check if table exists
#'
#' Check if table exists in the data registry
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#'
#' @return Returns \code{TRUE} if a table exists, \code{FALSE} if it doesn't
#'
check_table_exists <- function(table) {
  if (!is.character(table))
    stop("Table must be a string")

  tables <- get_tables()

  if (!table %in% tables)
    return(FALSE)
  return(TRUE)
}
