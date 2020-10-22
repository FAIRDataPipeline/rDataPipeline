#' create_estimate
#'
#' Function to populate toml file with point-estimate type data, *e.g.*
#' \itemize{
#' \item{Generate a single toml file containing a single point-estimate component}
#' \item{Generate a single toml file containing multiple point-estimate components}
#' }
#' If a file already exists at the specified location, an error will be returned.
#'
#' @param filename a \code{string} specifying the name of the toml file
#' @param path a \code{string} specifying the directory in which you want to
#' save the toml file; this will be automatically generated if it doesn't
#' already exist
#' @param parameters a \code{list} (see Examples)
#'
#' @export
#'
#' @examples
#' filename <- "0.1.0.toml"
#' data_product_name <- "some/descriptive/name/asymptomatic-period"
#'
#' # Write a single estimate into a toml file
#' create_estimate(filename = filename,
#'                 path = data_product_name,
#'                 parameters = list(`asymptomatic-period` = 192.0))
#'
#' file.remove(file.path(data_product_name, filename))
#'
#' # Write multiple estimates into a toml file
#' create_estimate(filename = filename,
#'                 path = data_product_name,
#'                 parameters = list(`asymptomatic-period-1` = 192.0,
#'                                   `asymptomatic-period-2` = 190.2))
#'
#' file.remove(file.path(data_product_name, filename))
#'
create_estimate <- function(filename,
                            path,
                            parameters) {
  # Check filename is a toml
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # If path is missing, use working directory
  if(missing(path)) path <- getwd()

  # If file exists, stop
  if(file.exists(file.path(path, filename)))
    stop("File already exists at this location")

  # If path doesn't exist, generate directory structure
  if(!file.exists(path))
    dir.create(path, recursive = TRUE)

  # Write toml
  tmp <- lapply(seq_along(parameters), function(i) {
    paste0("[", names(parameters)[[i]], "]\ntype = \"point-estimate\"",
           "\nvalue = ", parameters[[i]], "\n")
  })

  cat(paste(tmp, collapse = "\n"), file = file.path(path, filename))
}
