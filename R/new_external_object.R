#' Post entry to external_object table
#'
#' Upload information to the \code{external_object} table in the data registry
#' 
#' @keywords internal
#'
#' @param doi_or_unique_name a \code{string} specifying the DOI or name of the
#' \code{external_object}
#' @param primary_not_supplement (optional) a boolean flag to indicate
#' whether the external object is the primary source (\code{TRUE}) or not
#' (\code{FALSE})
#' @param release_date the date-time that the \code{external_object} was
#' released *e.g.* \code{Sys.time()} or "2010-07-11 12:15:00 BST"
#' @param title a `string` specifying the title of the \code{external_object}
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{external_object}
#' @param data_product_url a \code{string} specifying the URL of an entry in
#' the \code{data_product} table
#' @param original_store_url (optional) a `string` specifying the URL of a
#' an entry in the \code{storage_location} table that references the original
#' location of an \code{external_object}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_external_object <- function(doi_or_unique_name,
                                primary_not_supplement = TRUE,
                                release_date,
                                title,
                                description,
                                data_product_url,
                                original_store_url,
                                endpoint = "http://127.0.0.1:8000/api/") {

  data <- list(doi_or_unique_name = doi_or_unique_name,
               release_date = release_date,
               title = title,
               data_product = data_product_url)

  if (!missing(primary_not_supplement))
    data$primary_not_supplement <- primary_not_supplement

  if (!missing(description))
    data$description <- description

  if (!missing(original_store_url))
    data$original_store <- original_store_url

  post_data(table = "external_object",
            data = data,
            endpoint = endpoint)
}
