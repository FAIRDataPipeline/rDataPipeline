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
#' @param type logical whether to include types this will return a dataframe
#'
#' @return a character vector of fields or a list of fields if filter fields is
#' set to "none"
#'
#' @export
#'
get_fields <- function(table, key, filter_fields = "all", type = FALSE){

  if(table == "users" | table == "groups")
    stop("users and groups are protected tables")

  h <- c(Authorization = paste("token", key))

  out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "", sep = "/"),
                    httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  # Return for all fields
  all <- names(out$actions$POST)
  all.type <- character(0)
  read_only <- character(0)
  read_only.type <- character(0)
  optional <- character(0)
  optional.type <- character(0)
  required <- character(0)
  required.type <- character(0)
  writable <- character(0)
  writable.type <- character(0)
  for(i in seq_along(out$actions$POST)){
    field <- out$actions$POST[i]
    all.type <- c(all.type, field[[1]]$type)
    if(field[[1]]$read_only){
      read_only <- c(read_only, names(field))
      read_only.type <- c(read_only.type, field[[1]]$type)
    }
    else{
      writable <- c(writable, names(field))
      writable.type <- c(writable.type, field[[1]]$type)
    }
    if(field[[1]]$required){
      required <- c(required, names(field))
      required.type <- c(required.type, field[[1]]$type)
    }
    else{
      optional <- c(optional, names(field))
      optional.type <- c(optional.type, field[[1]]$type)
    }
  }

  if(type)
  {
    if(filter_fields == "all"){
      df <- as.data.frame(cbind(all, all.type))
      names(df) <- c("field", "type")
      return(df)
    }
    if(filter_fields == "optional"){
      df <- as.data.frame(cbind(optional, optional.type))
      names(df) <- c("field", "type")
      return(df)
    }
    if(filter_fields == "read_only"){
      df <- as.data.frame(cbind(read_only, read_only.type))
      names(df) <- c("field", "type")
      return(df)
    }
    if(filter_fields == "writable"){
      df <- as.data.frame(cbind(read_only, read_only.type))
      names(df) <- c("field", "type")
      return(df)
    }
    if(filter_fields == "required"){
      df <- as.data.frame(cbind(required, required.type))
      names(df) <- c("field", "type")
      return(df)
    }
  }
  else{
    if(filter_fields == "all")
      return(all)
    if(filter_fields == "optional")
      return(optional)
    if(filter_fields == "read_only")
      return(read_only)
    if(filter_fields == "writable")
      return(writable)
    if(filter_fields == "required")
      return(required)
  }

  return(list(all=all, read_only=read_only, optional=optional, writable = writable))
}

