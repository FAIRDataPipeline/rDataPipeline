#' Check whether paper exists
#'
#' Check whether paper is in the data registry
#'
#' @param doi doi
#'
paper_exists <- function(doi) {
check_exists("external_object",
             list(doi_or_unique_name = paste0("doi://", doi)))
}
