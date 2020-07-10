#' new_issue
#'
#' @param severity
#' @param description
#' @param key
#'
#' @export
#'
new_issue <- function(severity,
                      description,
                      key) {

  post_data(table = "issue",
            data =  list(severity = severity,
                         description = description),
            key)
}
