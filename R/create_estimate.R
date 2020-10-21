#' create_estimate
#'
#' Function to populate toml file with point-estimate type data.
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#' @param path a \code{string} specifying the directory in which you want to
#' save the toml file; this will be automatically generated if it doesn't
#' already exist
#' @param parameters a \code{list} (see Examples)
#'
#' @export
#'
#' @examples
#' # Write a single estimate into a toml file
#' filename <- "test.toml"
#'
#' create_estimate(filename = filename,
#'                 path = ".",
#'                 parameters = list(`asymptomatic-period`` = 192.0))
#'
#' file.remove(filename)
#'
#' # Write multiple estimates into a toml file
#' filename <- "anothertest.toml"
#'
#' create_estimate(filename = filename,
#'                 path = ".",
#'                 parameters = list(`asymptomatic-period`` = 192.0,
#'                                   `standard-deviation`` = 10.2))
#'
#' file.remove(filename)
#'
create_estimate <- function(filename,
                            path,
                            parameters) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # Generate directory structure
  if(missing(path)) path <- getwd()
  else if(!file.exists(path)) dir.create(path, recursive = TRUE)

  # Write toml
  tmp <- lapply(seq_along(parameters), function(i) {
    paste0("[", names(parameters)[[i]], "]\ntype = \"point-estimate\"",
           "\nvalue = ", parameters[[i]], "\n")
  })

  cat(paste(tmp, collapse = "\n"), file = file.path(path, filename))
}
