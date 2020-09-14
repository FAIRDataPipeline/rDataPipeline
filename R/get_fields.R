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
  out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "",
                                     sep = "/"),
                    httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)


  lapply(seq_along(out$actions$POST), function(i) {

    field <- out$actions$POST[i]

    name <- names(field)
    data_type <- field[[1]]$type
    min_value <- NA
    max_value <- NA
    choice_values <- NA
    choice_names <- NA

    read_only <- dplyr::if_else(field[[1]]$read_only, TRUE, FALSE)
    required <- dplyr::if_else(field[[1]]$required, TRUE, FALSE)


    if("min_value" %in% names(field[[1]])) {
      min_value <- field[[1]]$min_value
    } else {
      min_value <- NA
    }

    if("max_value" %in% names(field[[1]])) {
      max_value <- field[[1]]$max_value
    } else {
      max_value <- NA
    }


    if("choices" %in% names(field[[1]])){
      if(is.list(field[[1]]$choices)){
        for(choice in seq_along(field[[1]]$choices)){
          choice_values <- dplyr::if_else(is.na(choice_values), as.character(field[[1]]$choices[[choice]]$value), paste(choice_values, field[[1]]$choices[[choice]]$value, sep = ", "))
          choice_names <- dplyr::if_else(is.na(choice_names), field[[1]]$choices[[choice]]$display_name, paste(choice_names, field[[1]]$choices[[choice]]$display_name, sep = ", "))
        }
      }
    }
    data.frame(field = name,
               data_type = data_type,
               read_only = read_only,
               required = required,
               min_value = min_value,
               max_value = max_value,
               choice_values = choice_values,
               choice_names = choice_names,
               stringsAsFactors = FALSE)
  }) %>% (function(x){do.call(rbind.data.frame, x)})

}
