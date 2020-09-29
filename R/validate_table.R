#' validate_table
#'
#' Function to validate table name
#'
#' @param table table name as character
#' @param key API Token as character
#'
#' @param online \code{boolean} if FALSE will not check if data registry objects exist
#'
#' @export
#'
validate_table <- function(table, key) {
  if(missing(table)) stop("table is required")
  if(table == "users" | table == "groups")
    stop("users and groups tables are read-only ")
  if(!check_table_exists(table)) stop("table must exist")
  table
}
