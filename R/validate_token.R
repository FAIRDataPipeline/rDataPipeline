#' validate_token
#' Function to validate user token
#'
#' @param key API Token as character
#'
#' @export
#'
validate_token <- function(key){
  if(missing(key))
    stop("API Token (key) must be supplied")
  if(!is.character(key))
    stop("API Token (key) must be a character")
  if(is.null(key))
    stop("API Token (key) cannot be null")
  if(key == "")
    stop("API Token (key) must contain a value")

  #todo validate the key with the data registry
  key
}
