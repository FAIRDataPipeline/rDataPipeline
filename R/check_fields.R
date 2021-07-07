#' check_fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing the query
#' @param endpoint endpoint
#'
#' @export
#' @keywords internal
#'
check_fields <- function(table, query, endpoint) {

  fields <- names(query)

  expected_types <- get_fields(table = table, endpoint = endpoint) %>%
    dplyr::filter(.data$field %in% fields) %>%
    dplyr::arrange(factor(.data$field, levels = fields)) %>%
    dplyr::select(.data$data_type) %>%
    unlist() %>% unname()

  lapply(seq_along(expected_types), function(x) {

    this_type <- expected_types[x]
    this_query <- query[[x]]
    query_class <- class(this_query)
    if (is.list(query_class))
      query_class <- class(query_class[[1]])

    # field ------------------------------------------------------------------

    if (this_type == "field") {
      output <- check_field(fields = fields[x],
                            query_class = query_class,
                            this_query = this_query,
                            endpoint = endpoint)

      # datetime -------------------------------------------------------------

    } else if (this_type == "datetime") {
      output <- check_datetime(query_class, this_query)

      # string ------------------------------------------------------------

    } else if (this_type == "string") {
      output <- check_string(this_query)

      # integer ------------------------------------------------------------

    } else if (this_type == "integer") {
      output <- check_integer(query_class, this_query)

      # boolean ------------------------------------------------------------

    } else if (this_type == "boolean") {
      output <- dplyr::if_else(query_class == "logical", TRUE, FALSE)

      # choice -------------------------------------------------------------

    } else if (this_type == "choice") {
      output <- dplyr::if_else(any(0:1 %in% this_query), TRUE, FALSE)

      # url ----------------------------------------------------------------

    } else if (this_type == "url") {
      output <- dplyr::if_else(grepl("http", this_query), TRUE, FALSE)

      # --------------------------------------------------------------------

    } else {
      stop("Unknown data type - has the data registry schema been edited?")
    }

    output

  }) %>% unlist()
}

check_field <- function(fields, query_class, this_query, endpoint) {
  if (is.character(this_query)) {

    if (grepl(endpoint, this_query)) {
      return(TRUE)

    } else if (!grepl("\\D", this_query)) {
      return(TRUE)

    } else {
      return(FALSE)

    }

  } else if (is.numeric(this_query)) {
    return(TRUE)

  } else {
    stop(paste(fields, query_class, class(this_query), ":", this_query))
    return(FALSE)

  }
}

check_datetime <- function(query_class, this_query) {
  if (query_class %in% "POSIXct") {
    return(TRUE)

  } else if (is.character(this_query)) {
    is_date <- tryCatch({
      as.Date(this_query)
    },
    error = function(e) {
      NULL
    })

    if (!is.null(is_date)) {
      return(TRUE)
    } else {
      return(FALSE)
    }

  } else {
    return(FALSE)
  }

}

check_string <- function(this_query) {
  dplyr::if_else(is.character(this_query), TRUE, FALSE)
}

check_integer <- function(query_class, this_query) {
  test <- (query_class == "numeric") &
    (this_query - round(this_query) < 0.0000001)
  dplyr::if_else(test, TRUE, FALSE)
}

flag <- function() {
  NULL
}
