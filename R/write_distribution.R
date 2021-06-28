#' Write distribution-type TOML file
#'
#' Function to populate toml file with distribution type data, *e.g.*
#' \itemize{
#' \item{Generate a single toml file containing a single distribution component}
#' \item{Generate a single toml file containing multiple distribution components}
#' \item{Generate a single toml file containing a single distribution component
#' and associated point-estimate components}
#' }
#' If a file already exists at the specified location, an error will be returned.
#'
#' @param filename a \code{string} specifying the name of the toml file
#' @param path a \code{string} specifying the directory in which you want to
#' save the toml file; this will be automatically generated if it doesn't
#' already exist
#' @param distribution a \code{list} containing 3 named elements:
#' \itemize{
#'  \item{name} - {a \code{string} specifying the name of the parameter}
#'  \item{distribution} - {a \code{string} specifying the name of the distribution}
#'  \item{parameters} - {a \code{list} specifying parameter values and associated
#'  names}
#' }
#' If more than one distribution is required, they must be added as a list
#' (see Examples)
#'
#' @family create functions
#'
#' @export
#'
#'
#' @examples
#' filename <- "0.1.0.toml"
#' data_product_name <- "sonia/latency-period"
#'
#' # Write a single distribution into a toml file
#' dist <- list(name = "latency-period",
#'              distribution = "gamma",
#'              parameters = list(shape = 2.0, scale = 3.0))
#'
#' create_distribution(filename = filename,
#'                                 path = data_product_name,
#'                                 distribution = dist)
#'
#' file.remove(file.path(data_product_name, filename))
#'
#' # Write multiple distributions into a toml file
#' dist1 <- list(name = "latency-period1",
#'                     distribution = "gamma",
#'                     parameters = list(shape = 2.0, scale = 3.0))
#' dist2 <- list(name = "latency-period2",
#'                     distribution = "gamma",
#'                     parameters = list(shape = 2.0, scale = 3.0))
#'
#' create_distribution(filename = filename,
#'                                path = data_product_name,
#'                                distribution = list(dist1, dist2))
#'
#' file.remove(file.path(data_product_name, filename))
#'
#' # Write a single distribution with point-estimates into a toml file
#' dist <- list(name = "latency-period",
#'                   distribution = "gamma",
#'                   parameters = list(shape = 2.0, scale = 3.0))
#' estimate1 <- list(mean = 1.0)
#' estimate2 <- list(`standard-deviation` = 1.0)
#'
#' create_distribution(filename = filename,
#'                                path = data_product_name,
#'                                distribution = list(dist, estimate1, estimate2))
#'
#' file.remove(file.path(data_product_name, filename))
#'
write_distribution <- function(filename,
                               path,
                               distribution) {

  # Checks ------------------------------------------------------------------

  check_yaml_for_write(handle, data_product)

  # Get metadata ------------------------------------------------------------


  # Check filename is a toml
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # If path is missing, use working directory
  if(missing(path)) path <- getwd()

  # If file already exists, stop
  if(file.exists(file.path(path, filename)))
    stop("File already exists at this location")

  # If path doesn't exist, generate directory structure
  if(!file.exists(path))
    dir.create(path, recursive = TRUE)

  # If only a single distribution was input, put it (a list) in a list so that
  # the following code works
  if(!is.null(names(distribution)))
    distribution <- list(distribution)

  # Prepare to write toml
  write_this <- lapply(seq_along(distribution), function(i) {
    this_distribution <- distribution[[i]]

    if("distribution" %in% names(this_distribution)) {
      parameters <- this_distribution$parameters
      tmp <- sapply(seq_along(parameters), function(x)
        paste0(names(parameters[x]), " = ", parameters[x])) %>%
        paste0(collapse = "\n")

      out <- paste0("[", this_distribution$name,
                    "]\ntype = \"distribution\"\ndistribution = \"",
                    this_distribution$distribution, "\"\n", tmp, "\n")

    } else {
      out <- paste0("[", names(this_distribution), "]\ntype = \"point-estimate\"",
                    "\nvalue = ", this_distribution, "\n")
    }
    out
  })

  # Write toml
  cat(paste(write_this, collapse = "\n"), file = file.path(path, filename))
}
