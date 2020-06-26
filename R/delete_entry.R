#' delete_entry
#'
#' @param url
#' @param key
#'
#' @export
#'
delete_entry <- function(url,
                         key) {

  h <- c(Authorization = paste("token", key))

  DELETE(url,
         httr::add_headers(.headers = h),
         verbose())
}
