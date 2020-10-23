#' validate_token
#' Function to validate user token
#'
#' @param key API Token as character
#'
#' @export
#'
#' @keywords internal
#'
validate_token <- function(key){
  if(missing(key))
    stop("API Token (key) must be supplied")
  if(!is.character(key))
    stop("API Token (key) must be a character")
  if(is.null(key))
    stop("API Token (key) cannot be null")
  if(length(key) > 1)
    stop("API Token must be a collapsed character")
  if(!nchar(key) == 40)
    stop("invalid key length")
  if(! grepl("^[a-zA-Z0-9]+$", key))
    stop("API token cannot contain special characters")

  out <- NULL

  tryCatch({

    table <- "issue"
    # Add token to options request header
    h <- c(Authorization = paste("token", key))

    # Perform an options request
    out <- httr::VERB("OPTIONS", paste("https://data.scrc.uk/api", table, "",
                                       sep = "/"),
                      httr::add_headers(.headers = h)) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)

  }, error = function(e){
    stop("something went wrong, please check your API token and try again")
  })

  if(any(names(out) == "detail")){
    stop(out["detail"])}


  #todo validate the key with the data registry
  key
}
