#' raise_issue
#'
#' @param index index returned from `link_*()`, `read_()`, or `write()`
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param component a \code{string} specifying the component name
#' @param data_product a \code{string} specifying the data product name
#' @param issue a \code{string} specifying the issue
#' @param severity a \code{numeric} value specifying the severity
#' @param whole_object a \code{boolean} flag specifying whether or not to
#' reference the whole_object
#'
#' @export
#'
raise_issue <- function(index,
                        handle,
                        component = NA,
                        data_product,
                        issue,
                        severity,
                        whole_object = FALSE) {

  if (missing(index)) {
    index <- NA
    reads <- handle$yaml$read
    writes <- handle$yaml$write

    for (i in seq_along(data_product)) {
      for (j in seq_along(component)) {

        this_data_product <- data_product[i]
        this_component <- component[j]

        if (!is.null(reads)) {
          if (this_data_product %in%
              unlist(lapply(reads, function(x) x$data_product))) {
            metadata <- resolve_read(handle, this_data_product)
          } else if (this_data_product %in%
                     unlist(lapply(writes, function(x) x$data_product))) {
            metadata <- resolve_write(handle, this_data_product)
          } else {
            stop("dataproduct not in config file")
          }
        }

        use_version <- metadata$version
        use_namespace <- metadata$namespace

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

    if (length(data_product) > 1 | length(component) > 1)
      usethis::ui_done("Recording issues in handle")

  } else {

    for (j in seq_along(index)) {
      this_index <- index[j]
      index_inputs <- which(handle$inputs$index %in% this_index)
      index_outputs <- which(handle$outputs$index %in% this_index)

      if (length(index_inputs) != 0) {
        tmp <- handle$inputs[index_inputs, ]

      } else if (length(index_outputs) != 0) {
        tmp <- handle$outputs[index_outputs, ]

      } else {
        usethis::ui_oops(
          paste("Issue not attached: data must be referenced in the",
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
