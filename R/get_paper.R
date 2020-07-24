#' get_paper
#'
#' @param doi
#'
#' @export
#'
get_paper <- function(doi) {
  get_url("external_object",
               list(doi_or_unique_name = paste0("doi://", doi)))
}
