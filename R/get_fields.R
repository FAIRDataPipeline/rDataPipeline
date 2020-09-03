#' Get fields from table
#'
#' Use API endpoint to produce a list of fields for a table.
#'
#' Requires API key
#'
#' @param table name of table
#' @param key api key / token
#' @return a dataframe of fields and their attributes
#' set to "none"
#'
#' @export
#'
get_fields <- function(table, key){

  # Users and Groups are valid tables but cannot be posted to
  if(table == "users" | table == "groups")
    stop("users and groups are protected tables")

  # Add token to options request header
  h <- c(Authorization = paste("token", key))

  # Perform an options request
  out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "", sep = "/"),
                    httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  # Set up data frame with column names
    df <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(df) <- c("field", "data_type", "read_only", "required", "min_value", "max_value", "choice_values", "choice_names")

  for(i in seq_along(out$actions$POST)){
    field <- out$actions$POST[i]
    name <- names(field)
    data_type <- field[[1]]$type
    read_only <- FALSE
    required <- FALSE
    min_value <- NA
    max_value <- NA
    choice_values <- NA
    choice_names <- NA

    if(field[[1]]$read_only){
      read_only <- TRUE
    }
    if(field[[1]]$required){
      required <- TRUE
    }
    else{
      required <- FALSE
    }
    if("min_value" %in% names(field[[1]])){
      min_value <- field[[1]]$min_value
    }
    if("max_value" %in% names(field[[1]])){
      max_value <- field[[1]]$max_value
    }
    if("choices" %in% names(field[[1]])){
      if(is.list(field[[1]]$choices)){
        choice_values <- NULL
        choice_names <- NULL
        for(choice in seq_along(field[[1]]$choices)){
          choice_values <- c(choice_values, field[[1]]$choices[[choice]]$value)
          choice_names <- c(choice_names, field[[1]]$choices[[choice]]$display_name)
        }
      }
    }
    df <- rbind(df,
                list(field = name,
                 data_type = data_type,
                 read_only = read_only,
                 required = required,
                 min_value = min_value,
                 max_value = max_value,
                 choice_values = paste(choice_values, sep ="", collapse = ", "),
                 choice_names = paste(choice_names, sep ="", collapse = ", ")))
  }

  df
}

