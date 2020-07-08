#' new_issue
#'
#' @param id
#' @param description
#' @param severity
#'
#' @export
#'
new_issue <- function(id,
                      description,
                      severity) {

  post_data(
    table = "source",
    data =  list(id = id,
                 description = description,
                 severity = severity))
}
