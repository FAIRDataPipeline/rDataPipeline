#' raise_issue
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
#' @export
#'
raise_issue <- function(index,
                        handle,
                        component,
                        data_product,
                        version,
                        namespace,
                        issue,
                        severity) {

  if (missing(index)) {
    index <- NA
    data_product <- data_product
    use_data_product <- data_product
    use_component <- component
    use_version <- version
    use_namespace <- namespace

  } else {
    index_inputs <- which(handle$inputs$index %in% index)
    index_outputs <- which(handle$outputs$index %in% index)

    if (length(index_inputs) != 0) {
      tmp <- handle$inputs[index_inputs,]

    } else if (length(index_outputs) != 0) {
      tmp <- handle$outputs[index_outputs,]

    } else {
      usethis::ui_oops(paste("Issue not attached: data must be referenced in the",
                             usethis::ui_value("config.yaml"), "file"))
      return(invisible(NULL))
    }

    data_product <- tmp$data_product
    use_data_product <- tmp$use_data_product
    use_component <- tmp$use_component
    use_version <- tmp$use_version
    use_namespace <- tmp$use_namespace
  }

  handle$raise_issue(index = index,
                     use_data_product = use_data_product,
                     use_component = use_component,
                     use_version = use_version,
                     use_namespace = use_namespace,
                     issue = issue,
                     severity = severity)

  usethis::ui_done("Recording issue in handle")
}
