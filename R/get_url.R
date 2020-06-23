#' get_url
#'
#' @param table
#' @param key GitHub key
#'
#' @export
#'
get_url <- function(table, key) {

  available <- get_existing(table = table)

  if(any(key %in% available)) {
    return(get_existing(table = table, key = key)$url)

  } else {
    stop(
      paste(key, "does not exist. Please select from the following options",
            "or create a new entry using new_", table, "():\n",
            paste(available, collapse = "\n"))
    )
  }
}
