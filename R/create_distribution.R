#' create_distribution
#'
#' Function to populate toml file with distribution type data.
#'
#' @param toml_filename a \code{string} specifying the name of the toml file
#' @param distribution a \code{string} specifying the name of the distribution
#' @param values a \code{numeric} \code{vector} specifying parameter values
#' @param names a \code{vector} specifying the name of each parameter value
#'
#' @export
#'
create_distribution <- function(toml_filename,
                                distribution,
                                values,
                                names) {
  # Check file name
  if(!(grepl(".toml$", toml_filename))) stop("toml_filename must be *.toml")

   # Write toml
  tmp <- sapply(seq_along(values), function(x)
    paste0(names[x], " = ", values[x], "\n")) %>%
    paste0(collapse = "")

  cat(paste0("[distribution]\ndistribution = \"", distribution, "\"\n",
             tmp, "\n"), file = toml_filename)
}
