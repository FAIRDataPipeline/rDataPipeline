#' paper_exists
#'
#' @export
#'
paper_exists <- function(doi) {
check_exists("external_object",
             list(doi_or_unique_name = paste0("doi://", doi)))
}
