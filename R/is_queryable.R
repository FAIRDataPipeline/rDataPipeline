#' Check whether fields are queryable
#'
#' Check whether fields are queryable
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing the query
#' @param method a \code{string} specifying the method, c("GET", "POST")
#' @param endpoint endpoint
#'
#' @return Returns \code{TRUE} if the entry is queryable and \code{FALSE} if it
#' isn't
#'
is_queryable <- function(table, query, method, endpoint) {

  if (table == "users") {
    return(NULL)
  } else if (table == "groups") {
    usethis::ui_stop("Can't query {ui_field(table)} table")
  }

  # Check whether field names are valid
  fields <- names(query)
  valid_fields <- all(fields %in%
                        get_table_queryable(table = table,
                                            endpoint = endpoint))

  # Check whether the class of each field in the query matches what is expected
  valid_query <- check_fields(table = table,
                              query = query,
                              method = method,
                              endpoint = endpoint)
  valid_query <- !(any(valid_query == FALSE))

  # Output
  if (!check_table_exists(table)) {
    usethis::ui_stop("{ui_field(table)} does not exist")

  } else if (any(is.character(fields) & fields == "")) {
    # An empty string is not a valid field
    usethis::ui_stop("{ui_field(fields)} contains invalid fields")

  } else if (!valid_fields) {
    usethis::ui_stop("{ui_field(fields)} contains invalid fields")

  } else if (!valid_query) {
    usethis::ui_stop("query contains fields of incorrect class")

    # msg <- vapply(seq_len(length(invalid_f)), function(y) {
    #   num_fields <- length(invalid_f)
    #
    #   if (num_fields == 2 & y == 2) {
    #     prepend <- " and "
    #   } else if (num_fields > 2) {
    #     prepend <- dplyr::if_else(y == num_fields, ", and ",
    #                               dplyr::if_else(y == 1, "", ", "))
    #   } else
    #     prepend <- ""
    #
    #   paste0(prepend, invalid_f[y], " (",
    #          invalid_t[y], ")")
    # }, character(1)) %>%
    #   paste(collapse = "")
    #
    # tmp <- paste("Invalid query - please check the following fields are of",
    #              "the correct class:\n   {msg}")
    # usethis::ui_stop(tmp)

  } else {
    return(TRUE)
  }
}
