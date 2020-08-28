#' validate_post_data
#' Function to validate post data
#'
#' @param table table name as character
#' @param data data as a named list
#'
#' @export
#'
validate_post_data <- function(table, data, key){

  # Validate Table Name
  if(missing(table))
    stop("table is required")
  if(table == "users" | table == "groups")
    stop("users and groups tables are read-only ")
  if(!check_table_exists(table))
    stop("table must exist")

  # validate data

  if(!is.list(data))
    stop("data must be a list")

  # ensure fields are writable
  table.writable <- get_table_writable(table, key)
  table.required <- get_table_required(table, key)
  data.field_names <- names(data)

  if(!all(data.field_names %in% table.writable))
    stop(paste0("Invalid field names valid names are: ", paste0(table.writable, collapse = " ")))

  if(!all(table.required %in% data.field_names))
    stop(paste0("required fields are missing required field names are: ", paste0(table.required, collapse = " ")))

   data

}
