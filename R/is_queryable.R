#' is_queryable
#'
#' Check whether fields are queryable
#'
#' @param table a \code{string} specifying the name of the table
#' @param query_parameter a \code{string} or \code{vector} of field names
#'
#' @return Returns \code{TRUE} if the entry is queryable and \code{FALSE} if it
#' isn't
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Is "name" a queryable field in the "storage_root" table?
#' is_queryable("storage_root", "name")
#'
#' # Are "not_a_field" and "name" queryable fields in the "storage_root" table?
#' is_queryable("storage_root", c("not_a_field", "name"))
#'
#' # Is "name" a queryable field in the "not_a_table" table?
#' is_queryable("users", "name")
#'
is_queryable <- function(table, query_parameter) {
  if(table == "users" | table == "groups")
    return(FALSE) # only queryable with token
  if(is.null(get_table_queryable(table)))
    return(FALSE)
  return(query_parameter %in% get_table_queryable(table))
}
