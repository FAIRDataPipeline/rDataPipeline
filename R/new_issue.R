#' new_issue
#'
#' @param severity
#' @param description
#'
#' @export
#'
new_issue <- function(severity,
                      description) {

  post_data(table = "issue",
            data =  list(severity = severity,
                         description = description),
            key)
}
