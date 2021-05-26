#' issue_with_component
#'
#' @param component_id component_id
#' @param handle handle
#' @param component component
#' @param data_product data_product
#' @param version version
#' @param namespace namespace
#' @param issue issue
#' @param severity severity
#'
issue_with_component <- function(component_id,
                                 handle,
                                 component,
                                 data_product,
                                 version,
                                 namespace,
                                 issue,
                                 severity) {

  if (missing(component_id)) {
    component_id <- NA

  } else {
    index_inputs <- which(handle$inputs$index %in% component_id)
    index_outputs <- which(handle$outputs$index %in% component_id)

    if (length(index_inputs) != 0) {
      tmp <- handle$inputs[index_inputs,]
      namespace <- handle$yaml$run_metadata$default_input_namespace

    } else if (length(index_outputs) != 0) {
      tmp <- handle$outputs[index_outputs,]
      namespace <- handle$yaml$run_metadata$default_output_namespace

    } else {
      usethis::ui_oops(paste("Issue not attached: data must be referenced in the",
                             usethis::ui_value("config.yaml"), "file"))
      return(invisible(NULL))
    }

    component <- tmp$component
    data_product <- tmp$data_product
    version <- tmp$version
  }

  handle$raise_issue_component(component_id = component_id,
                               component = component,
                               data_product = data_product,
                               version = version,
                               namespace = namespace,
                               issue = issue,
                               severity = severity)

  usethis::ui_done("Recording issue in handle")
}
