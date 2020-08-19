#' Get github Package Version
#'
#' @return Returns the current package version
#'
#' @export
#'
get_remote_package_version <- function(repo = "ScottishCovidResponse/SCRCdataAPI",
                                       branch = "master"){
  description <- utils::read.delim(
    file.path("https://raw.githubusercontent.com", repo, branch,
              "DESCRIPTION"), sep = ":",
    header = FALSE, row.names = 1)
  if(any(row.names(description) == "Version"))
    return(gsub(" ", "", description['Version',]))
  else
    stop("Something went wrong")
}
