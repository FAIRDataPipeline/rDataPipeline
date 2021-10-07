#' Patch entry in data registry
#'
#' @param url url
#' @param data data
#'
#' @keywords internal
#'
patch_data <- function(url,
                       data) {

  key <- get_token()
  h <- c(Authorization = paste("token", key))

  result <- httr::PATCH(url,
                        body = jsonlite::toJSON(data, pretty = T,
                                                auto_unbox = T,
                                                force = T),
                        httr::content_type("application/json"),
                        httr::add_headers(.headers = h),
                        verbose())

  tmp <- result %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if (result$status != 200)
      stop("Adding new data returned non-200 status code:", tmp)
}
