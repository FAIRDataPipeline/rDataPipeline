#' issue_with_externalobject
#'
#' @param handle \code{fdp} object
#' @param external_object a \code{string} specifying the name of the external
#' object
#' @param version a \code{string} specifying the version of the external object
#' @param issue a \code{string} describing the issue
#' @param severity a \code{numeric} value specifying the severity of the issue
#'
issue_with_externalobject <- function(handle,
                                      external_object,
                                      version,
                                      issue,
                                      severity) {

  handle$raise_issue(index = NA,
                     component = NA,
                     data_product = NA,
                     external_object = external_object,
                     version = version,
                     namespace = NA,
                     issue = issue,
                     severity = severity)

  usethis::ui_done("Noted issue in handle")
}
