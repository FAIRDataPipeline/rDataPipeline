#' Get github Package Version
#'
#' @param repo repo name
#'
#' @return Returns the current package version
#'
get_remote_package_version <- function(
  repo = "ScottishCovidResponse/SCRCdataAPI"){
  description <- as.character(0)
  tryCatch({
    description <- utils::read.delim(
      file.path("https://raw.githubusercontent.com", repo, "master",
                "DESCRIPTION"), sep = ":",
      header = FALSE, row.names = 1)
  }, error = function(e){
    stop("Problem with github repository please check the package name")}, warning = function(w){}
  )

  if(any(row.names(description) == "Version"))
    return(gsub(" ", "", description['Version',]))
  else
    stop("Something went wrong")
}
