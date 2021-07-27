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
#' @param whole_object whole_object
#'
#' @export
#'
raise_issue <- function(index,
                        handle,
                        component = NA,
                        data_product,
                        version,
                        namespace,
                        issue,
                        severity,
                        whole_object = FALSE) {

  if (missing(index)) {
    index <- NA
    use_version <- version
    use_namespace <- namespace

    for (i in seq_along(data_product)) {
      for (j in seq_along(component)) {
        this_data_product <- data_product[i]
        this_component <- component[j]

        handle$raise_issue(index = index,
                           type = "data",
                           use_data_product = this_data_product,
                           use_component = this_component,
                           use_version = use_version,
                           use_namespace = use_namespace,
                           issue = issue,
                           severity = severity)
      }
    }

    if (length(data_product) > 1 | length(component) > 1) {
      usethis::ui_done("Recording issues in handle")
    } else {
      usethis::ui_done("Recording issue in handle")
    }

  } else {

    for (j in seq_along(index)) {
      this_index <- index[j]
      index_inputs <- which(handle$inputs$index %in% this_index)
      index_outputs <- which(handle$outputs$index %in% this_index)

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

      if (whole_object) {
        use_component <- NA
      } else {
        use_component <- tmp$use_component
      }

      use_version <- tmp$use_version
      use_namespace <- tmp$use_namespace

      handle$raise_issue(index = this_index,
                         type = "data",
                         use_data_product = use_data_product,
                         use_component = use_component,
                         use_version = use_version,
                         use_namespace = use_namespace,
                         issue = issue,
                         severity = severity)
    }

    if (length(index) == 1) {
      usethis::ui_done("Recording issue in handle")
    } else {
      usethis::ui_done("Recording issues in handle")
    }
  }
}
