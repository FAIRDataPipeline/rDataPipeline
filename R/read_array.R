#' Read component from array-type H5 file
#'
#' Function to read array type data from hdf5 file.
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @return Returns an array with attached \code{Dimension_i_title},
#' \code{Dimension_i_units}, \code{Dimension_i_values}, and \code{units}
#' attributes, if available
#'
#' @export
#'
read_array <- function(handle,
                       data_product,
                       component) {

  read <- handle$yaml$read

  # If names(read) is not null, then a single entry has been added that
  # is not in a list, so put it in a list
  if (!all(is.null(names(read))))
    read <- list(read)

  # Find data product in `read:` section of config.yaml
  index <- lapply(seq_along(read), function(x)
    read[[x]]$data_product == data_product &&
      read[[x]]$component == component) %>%
    unlist() %>%
    which()

  this_read <- read[[index]]
  version <- this_read$version

  if (any(names(this_read) == "namespace")) {
    namespace <- this_read$namespace
  } else {
    namespace <- handle$yaml$run_metadata$default_input_namespace
  }

  run_server()

  namespace_url <- get_entry("namespace", list(name = namespace))

  this_entry <- get_entry("data_product",
                          list(name = data_product,
                               version = version,
                               namespace = namespace_url))[[1]]

  this_object <- get_entity(this_entry$object)
  this_location <- get_entity(this_object$storage_location)

  stop_server()

  # Read hdf5 file
  path <- this_location$path
  datastore <- handle$yaml$run_metadata$default_data_store
  file.h5 <- rhdf5::h5read(paste0(datastore, path), component)

  # Extract data object
  object <- file.h5$array
  if(is.vector(object)) object <- t(matrix(object))

  # Extract dimension names and make sure they're in the right order
  ind <- grep("Dimension_[0-9]*_names", names(file.h5))
  tmp <- file.h5[ind]
  ord <- order(names(tmp))
  tmp <- tmp[ord]

  # Attach dimension names to the object
  for(i in seq_along(tmp)) {
    if(i == 1) {
      rownames(object) <- tmp[[i]]
    } else if(i == 2) {
      colnames(object) <- tmp[[i]]
    } else {
      dimnames(object)[[i]] <- tmp[[i]]
    }
  }

  # Attach remaining list elements as attributes
  ind <- grep("Dimension_[0-9]_names|array", names(file.h5))
  tmp <- file.h5[-ind]

  for(i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()
  object
}
