#' check_entry
#'
#' @param entry entry
#' @param table table
#' @param key key
#' @param convert_urls convert_urls
#'
#' @export
#'
check_entry <- function(entry,
                        table,
                        field,
                        key,
                        convert_urls = FALSE) {

  existing <- get_existing(table)

  if(entry %in% existing[[field]]) {
    output <- existing[existing[[field]] == entry,] %>%
      unlist() %>%
      as.list()

    if(convert_urls) {
      ind <- which(grepl("data.scrc.uk/api/", output))

      if(length(ind) > 0)
        for(j in ind)
          output[j] <- convert_url(url = unlist(output[j]), key)
    }

  } else {
    stop(paste0("source_data/", table, ": \"",
                entry, "\" does not exist. ",
                "Please generate a new database entry using the new_",
                table, "() function."))
  }

  output
}



