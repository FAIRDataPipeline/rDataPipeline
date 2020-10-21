#' create_distribution
#'
#' Function to populate toml file with distribution type data.
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#' @param path a \code{string} specifying the directory in which you want to
#' save the toml file; this will be automatically generated if it doesn't
#' already exist
#' @param distribution a \code{list} containing 3 named elements:
#' \itemize{
#'  \item{name}{a \code{string} specifying the name of the parameter}
#'  \item{distribution}{a \code{string} specifying the name of the distribution}
#'  \item{parameters}{a \code{list} specifying parameter values and associated
#'  names}
#' }
#' If more than one distribution is required, they must be added as a list
#' (see Examples)
#' @export
#'
#' @examples
#' # Write a single distribution into a toml file
#' dist <- list(name = "latency-period",
#'              distribution = "gamma",
#'              parameters = list(shape = 2.0, scale = 3.0))
#' filename <- "test_single.toml"
#'
#' create_distribution(filename = filename,
#'                     path = ".",
#'                     distribution = dist)
#'
#' file.remove(filename)
#'
#' # Write multiple distributions into a toml file
#' dist1 <- list(name = "latency-period",
#'               distribution = "gamma",
#'               parameters = list(shape = 2.0, scale = 3.0))
#' dist2 <- list(name = "virulence",
#'               distribution = "gamma",
#'               parameters = list(shape = 2.0, scale = 3.0))
#' filename <- "test_multi.toml"
#'
#' create_distribution(filename = filename,
#'                     path = ".",
#'                     distribution = list(dist1, dist2))
#'
#' file.remove(filename)
#'
create_distribution <- function(filename,
                                path,
                                distribution) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # Generate directory structure
  if(missing(path)) path <- getwd()
  else if(!file.exists(path)) dir.create(path, recursive = TRUE)

  # If only a single distribution was input, put it in a list so that the
  # following code works
  if(!is.null(names(distribution)))
    distribution <- list(distribution)

  # Write toml
  write_this <- lapply(seq_along(distribution), function(i) {
    this_distribution <- distribution[[i]]

    parameters <- this_distribution$parameters
    tmp <- sapply(seq_along(parameters), function(x)
      paste0(names(parameters[x]), " = ", parameters[x])) %>%
      paste0(collapse = "\n")

    paste0("[", this_distribution$name,
           "]\ntype = \"distribution\"\ndistribution = \"",
           this_distribution$distribution, "\"\n", tmp, "\n")
  })

  cat(paste(write_this, collapse = "\n"), file = file.path(path, filename))
}
