#' Read component from table-type H5 file
#'
#' Function to read table type data from hdf5 file.
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @return Returns a \code{data.frame} with attached \code{column_units}
#' attributes, if available
#'
#' @export
#'
read_table <- function(handle,
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

  run_server()

  this_entry <- get_entry("data_product",
                          list(name = data_product,
                               version = this_read$version,
                               namespace = this_read$namespace))[[1]]

  this_object <- get_entity(this_entry$object)
  this_location <- get_entity(this_object$storage_location)

  stop_server()

  # Read hdf5 file
  path <- this_location$path
  datastore <- handle$yaml$run_metadata$default_data_store
  file.h5 <- rhdf5::h5read(paste0(datastore, path), component)

  # Extract data object
  object <- file.h5$table

  # Attach rownames to object
  if(any("row_names" %in% names(file.h5)))
    rownames(object) <- file.h5$row_names

  # Attach remaining list elements as attributes
  ind <- grep("row_names|table", names(file.h5))
  tmp <- file.h5[-ind]

  for(i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()
  object <- data.frame(lapply(object, type.convert, as.is = TRUE))
  object
}
