##################################################################
##        table definitions and functions for validation        ##
##             table definitions updated 05/08/2020             ##
##################################################################

#' get tables from registry
#' use api endpoint to produce a list of tables
#'
#' @return a character vector of tables
#'
#' @export
#'
get_tables <- function(){
  out <- httr::GET(file.path("https://data.scrc.uk/api", "")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  return(names(out))
}

#' get fields from table
#' use api endpoint to produce a list of fields for a table
#' requires api key
#'
#' @param table name of table
#' @param key api key / token
#' @param filter_fields which fields you want returned
#' *e.g.* all, none, readable, writable, optional
#'
#' @return a character vector of fields or a list of fields if filter fields is set to "none"
#'
#' @export
#'
get_fields <- function(table, key, filter_fields="all"){
  h <- c(Authorization = paste("token", key))

  out <- httr::VERB("OPTIONS", file.path("https://data.scrc.uk/api", table, ""),
                    httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  all <- names(out$actions$POST)
  if(filter_fields == "all")
    return(all)
  read_only <- character(0)
  optional <- character(0)
  required <- character(0)
  for(i in seq_along(out$actions$POST)){
    field <- out$actions$POST[i]
    if(field[[1]]$read_only)
      read_only <- c(read_only, names(field))
    if(!field[[1]]$required)
      optional <- c(optional, names(field))
    else
      required <- c(required, names(field))
  }
  if(filter_fields == "optional")
    return(optional)
  if(filter_fields == "read_only")
    return(read_only)
  writable <- all[!is.element(all, read_only)]
  if(filter_fields == "writable")
    return(writable)
  if(filter_fields == "required")
  return(list(all=all, read_only=read_only, optional=optional, writable = writable))
}

#' check table exists
#'
#' @param table name of table
#'
#' @return boolean if a table exists
#'
#' @export
#'
check_table_exists <- function(table){
  if(! table %in% get_tables())
    return(FALSE)
  return(TRUE)
}

#' Get Writable Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a character vector of writable fields
#'
#' @export
#'
get_table_writable <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get_fields(table, key, "writable"))
}

#' Get Readable Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a character vector of readable fields
#'
#' @export
#'
get_table_readable <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get_fields(table, key, "all"))
}

#' Get Queryable Fields
#'
#' @param table name of table
#'
#' @return a character vector of queryable fields
#'
#' @export
#'
get_table_queryable <- function(table)
{
  out <- httr::VERB("OPTIONS", file.path("https://data.scrc.uk/api", table, "")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  return(as.character(out$filter_fields))
}


#' Get Optional Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a character vector of optional fields
#'
#' @export
#'
get_table_optional <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get_fields(table, key, "optional"))
}

#' Get Required Fields
#'
#' @param table name of table
#' @param key api key / token
#'
#' @return a character vector of required fields
#'
#' @export
#'
get_table_required <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get_fields(table, key, "required"))
}

#' is queryable
#' Produces error \code{Unknown Table} if table does not exist
#'
#' @param table name of table
#' @param query_parameter a string or vector of strings to check
#'
#' @return either true / false if a single string is provided or a vector or true or false if vector is provided
#'
#' @export
#'
is_queryable <- function(table, query_parameter)
{
  if(table == "users" | table == "groups")
    return(FALSE) # only queryable with token
  if(is.null(get_table_queryable(table)))
    return(FALSE)
  return(query_parameter %in% get_table_queryable(table))
}

#' check query
#' produces error if table does not exist or if query is not a list
#'
#' @param table name of table
#' @param query query to check
#'
#' @return boolean if the query is valid for the table
#'
#' @export
#'
check_query <-function(table, query){
  if(!is.list(query))
    stop("Invalid query type")
  if(length(query) == 0)
    return(TRUE)
  return(all(is_queryable(table, names(query))))
}


