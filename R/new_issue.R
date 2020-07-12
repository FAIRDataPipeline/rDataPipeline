#' new_issue
#'
#' @param severity e.g.
#' @param description e.g.
#' @param key key
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
