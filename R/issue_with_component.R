#' issue_with_component
#'
#' @param component_id component_id
#' @param handle handle
#' @param component component
#' @param data_product data_product
#' @param version version
#' @param issue issue
#' @param severity severity
#'
#' @return
#'
issue_with_component <- function(component_id,
                                 handle,
                                 component,
                                 data_product,
                                 version,
                                 issue,
                                 severity) {

  if (component_id %in% handle$inputs$index) {
    index <- which(handle$inputs$index %in% component_id)
    tmp <- handle$inputs[index,]
  }

  if (component_id %in% handle$outputs$index) {
    index <- which(handle$outputs$index %in% component_id)
    tmp <- handle$outputs[index,]
  }

  handle$raise_issue_component(component = tmp$component,
                               data_product = tmp$data_product,
                               version = tmp$version,
                               issue = issue,
                               severity = severity)

  usethis::ui_done("Noted issue in handle")
}
