#' Get remote (GitHub) package version
#'
#' @param repo a \code{string} specifying the github username/repository
#'
#' @return Returns the current package version
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_remote_package_version("ScottishCovidResponse/SCRCdataAPI")
#' }
#'
get_remote_package_version <- function(
  repo = "ScottishCovidResponse/SCRCdataAPI"){
  description <- as.character(0)
  tryCatch({
    url <- paste("https://raw.githubusercontent.com", repo, "master",
                 "DESCRIPTION", sep = "/")
    description <- readLines(url)
  }, error = function(e){
    stop("Problem with github repository please check the package name")},
  warning = function(w){}
  )

  if(any(grepl("Version", description))) {
    ind <- grep("Version", description)
    return(gsub("Version: ", "", description[ind]))
  } else
    stop("Something went wrong")
}
