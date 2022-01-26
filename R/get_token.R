#' get_token
#'
get_token <- function() {
  if (Sys.getenv("FDP_LOCAL_TOKEN") != "") {
    return(Sys.getenv("FDP_LOCAL_TOKEN"))
  }      
  readLines(file.path("~", ".fair", "registry", "token"))
}
