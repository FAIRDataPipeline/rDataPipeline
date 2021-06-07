#' issue_with_dataproduct
#'
#' Function to attach an issue to data product in handle. To be written to the
#' data registry with `finalise()`.
#'
#' An issue can be attached to a data product referenced by an `id` (returned
#' from `create_array()`) or
#'
#' @param index \code{numeric} value returned from
#' \code{create_array()}, referencing an input / output in the handle
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param version a \code{string} specifying the version of the data product
#' @param namespace a \code{string} specifying the namespace containing the
#' data product
#' @param issue a \code{string} describing the issue
#' @param severity a \code{numeric} value specifying the severity of the issue
#'
issue_with_dataproduct <- function(index,
                                   handle,
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

    data_product <- tmp$data_product
    version <- tmp$version
  }

  handle$raise_issue(index = index,
                     component = NA,
                     data_product = data_product,
                     external_object = NA,
                     version = version,
                     namespace = namespace,
                     issue = issue,
                     severity = severity)

  usethis::ui_done("Noted issue in handle")
}
