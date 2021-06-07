#' issue_with_component
#'
#' @param index component_id
#' @param handle handle
#' @param component component
#' @param data_product data_product
#' @param version version
#' @param namespace namespace
#' @param issue issue
#' @param severity severity
#'
issue_with_component <- function(index,
                                 handle,
                                 component,
                                 data_product,
                                 version,
                                 namespace,
                                 issue,
                                 severity) {

  if (missing(index)) {
    index <- NA

  } else {
    index_inputs <- which(handle$inputs$index %in% index)
    index_outputs <- which(handle$outputs$index %in% index)

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

  handle$raise_issue(index = index,
                     component = component,
                     data_product = data_product,
                     external_object = NA,
                     version = version,
                     namespace = namespace,
                     issue = issue,
                     severity = severity)

  usethis::ui_done("Recording issue in handle")
}
