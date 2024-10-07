#' Get fields from table
#'
#' Use API endpoint to produce a list of fields for a table. Requires API key.
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a \code{data.frame} of fields and their attributes set to
#' "none"
#'
get_fields <- function(table, endpoint = "http://127.0.0.1:8000/api/") {

  # Add token to options request header
  key <- get_token()
  h <- c(Authorization = paste("token", key))

  api_url <- paste0(endpoint, table)
  api_url <- file.path(dirname(api_url), basename(api_url), "")

  # Perform an options request
  out <- httr::VERB("OPTIONS", api_url,
                    httr::add_headers(.headers = h)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  lapply(seq_along(out$actions$POST), function(i) {

    field <- out$actions$POST[i]

    name <- names(field)
    data_type <- field[[1]]$type
    min_length <- NA
    max_length <- NA
    choice_values <- NA
    choice_names <- NA

    read_only <- dplyr::if_else(field[[1]]$read_only, TRUE, FALSE)
    required <- dplyr::if_else(field[[1]]$required, TRUE, FALSE)

    if ("min_length" %in% names(field[[1]])) {
      min_length <- field[[1]]$min_length
    } else {
      min_length <- NA
    }

    if ("max_length" %in% names(field[[1]])) {
      max_length <- field[[1]]$max_length
    } else {
      max_length <- NA
    }

    if ("choices" %in% names(field[[1]])) {
      if (is.list(field[[1]]$choices)) {
        for (choice in seq_along(field[[1]]$choices)) {
          choice_values <- dplyr::if_else(
            condition = is.na(choice_values),
            true = as.character(field[[1]]$choices[[choice]]$value),
            false = paste(choice_values, field[[1]]$choices[[choice]]$value,
                          sep = ", "))
          choice_names <- dplyr::if_else(
            condition = is.na(choice_names),
            true = field[[1]]$choices[[choice]]$display_name,
            false = paste(choice_names,
                          field[[1]]$choices[[choice]]$display_name,
                          sep = ", "))
        }
      }
    }
    data.frame(field = name,
               data_type = data_type,
               read_only = read_only,
               required = required,
               min_length = min_length,
               max_length = max_length,
               choice_values = choice_values,
               choice_names = choice_names,
               stringsAsFactors = FALSE)
  }) %>%
    (function(x) {
      do.call(rbind.data.frame, x)
    })

}
