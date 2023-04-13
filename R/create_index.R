#' create_index
#' 
#' @keywords internal
#'
#' @param self self
#'
create_index <- function(self) {

  tmp <- 1

  if (!is.null(self$inputs))
    tmp <- c(tmp, max(self$inputs$index) + 1)

  if (!is.null(self$outputs))
    tmp <- c(tmp, max(self$outputs$index) + 1)

  max(tmp)
}
