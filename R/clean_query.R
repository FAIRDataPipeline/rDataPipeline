#' clean_query
#'
#' @param data data
#'
#' @export
#'
clean_query <- function(data) {
  fields <- lapply(seq_along(data), function(x) {
    if(grepl("https://data.scrc.uk/api", data[[x]])) {
      tmp <- gsub("https://data.scrc.uk/api/", "", data[[x]])
      tmp <- gsub("users", "", tmp)
      tmp <- gsub("groups", "", tmp)
      tmp <- gsub("issue", "", tmp)
      tmp <- gsub("object", "", tmp)
      tmp <- gsub("object_component", "", tmp)
      tmp <- gsub("code_run", "", tmp)
      tmp <- gsub("storage_root", "", tmp)
      tmp <- gsub("storage_location", "", tmp)
      tmp <- gsub("source", "", tmp)
      tmp <- gsub("external_object", "", tmp)
      tmp <- gsub("quality_controlled", "", tmp)
      tmp <- gsub("keyword", "", tmp)
      tmp <- gsub("author", "", tmp)
      tmp <- gsub("licence", "", tmp)
      tmp <- gsub("namespace", "", tmp)
      tmp <- gsub("data_product", "", tmp)
      tmp <- gsub("code_repo_release", "", tmp)
      tmp <- gsub("key_value", "", tmp)
      tmp <- gsub("text_file", "", tmp)
      tmp <- gsub("/", "", tmp)
    } else data[[x]]
  })
  names(fields) <- names(data)
  fields
}
