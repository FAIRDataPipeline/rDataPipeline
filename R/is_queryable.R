#' Check whether fields are queryable
#'
#' Check whether fields are queryable
#'
#' @param table a \code{string} specifying the name of the table
#' @param query_parameter a \code{string} or \code{vector} of field names
#'
#' @return Returns \code{TRUE} if the entry is queryable and \code{FALSE} if it
#' isn't
#' @export
#' @keywords internal
#'
#' @examples
#' is_queryable("storage_root", "name")
#' is_queryable("storage_root", c("not_a_field", "name"))
#' is_queryable("not_a_table", "name")
#'
is_queryable <- function(table, query_parameter) {
  if(table == "users" | table == "groups")
    return(FALSE) # only queryable with token

  if(!check_table_exists(table)) {
    message("table doesn't exist")
    return(FALSE)
  }

  query_parameter %in% get_table_queryable(table)
}
