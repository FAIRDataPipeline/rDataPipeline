#' issue_with_dataproduct
#'
#' Function to attach an issue to data product in handle. To be written to the
#' data registry with `finalise()`.
#'
#' An issue can be attached to a data product referenced by an `id` (returned
#' from `create_array()`) or
#'
#' @param data_product_id \code{numeric} value returned from
#' \code{create_array()}, referencing an input / output in the handle
#' @param handle handle
#' @param data_product data_product
#' @param version version
#' @param issue issue
#' @param severity severity
#'
issue_with_dataproduct <- function(data_product_id,
                                   handle,
                                   data_product,
                                   version,
                                   issue,
                                   severity) {

  if (!missing(data_product_id)) {

    index_inputs <- which(handle$inputs$index %in% data_product_id)
    if (length(index_inputs) != 0)
      tmp <- handle$inputs[index_inputs,]

    index_outputs <- which(handle$outputs$index %in% data_product_id)
    if (length(index_outputs) != 0)
      tmp <- handle$outputs[index_outputs,]

    data_product <- tmp$data_product
    version <- tmp$version
  }

  handle$raise_issue_dataproduct(data_product = data_product,
                                 version = version,
                                 issue = issue,
                                 severity = severity)

  usethis::ui_done("Noted issue in handle")
}
