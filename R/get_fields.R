#' Get fields from table
#'
#' Use API endpoint to produce a list of fields for a table.
#'
#' Requires API key
#'
#' @param table name of table
#' @param key api key / token
#' @param filter_fields which fields you want returned *e.g.* all, none,
#' readable, writable, optional
#'
#' @return a character vector of fields or a list of fields if filter fields is
#' set to "none"
#'
#' @export
#'
get_fields <- function(table, key, filter_fields = "all"){

  if(table == "users" | table == "groups")
    stop("users and groups are protected tables")

  h <- c(Authorization = paste("token", key))

  out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "", sep = "/"),
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
    return(required)
  return(list(all=all, read_only=read_only, optional=optional, writable = writable))
}

