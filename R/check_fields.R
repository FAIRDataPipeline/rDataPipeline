#' check_fields
#' 
#' @keywords internal
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing the query
#' @param method a \code{string} specifying the method, c("GET", "POST")
#' @param endpoint endpoint
#'
check_fields <- function(table, query, method, endpoint) {

  fields <- names(query)

  expected_types <- get_fields(table = table, endpoint = endpoint) %>%
    dplyr::filter(.data$field %in% fields) %>%
    dplyr::arrange(factor(.data$field, levels = fields)) %>%
    dplyr::select("data_type") %>%
    unlist() %>%
    unname()

  lapply(seq_along(expected_types), function(x) {

    this_field <- fields[x]
    this_type <- expected_types[x]
    this_query <- query[[x]]
    query_class <- class(this_query)

    if (is.list(query_class))
      query_class <- class(query_class[[1]])

    # field ------------------------------------------------------------------

    if (this_type == "field") {
      output <- check_field(table = table,
                            this_field = this_field,
                            query_class = query_class,
                            this_query = this_query,
                            method = method,
                            endpoint = endpoint)

      # datetime -------------------------------------------------------------

    } else if (this_type == "datetime") {
      output <- check_datetime(table = table,
                               this_field = this_field,
                               query_class = query_class,
                               this_query = this_query)

      # string ------------------------------------------------------------

    } else if (this_type == "string") {
      output <- check_string(table = table,
                             this_field = this_field,
                             this_query = this_query)

      # integer ------------------------------------------------------------

    } else if (this_type == "integer") {
      output <- check_integer(table = table,
                              this_field = this_field,
                              query_class = query_class,
                              this_query = this_query)

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
