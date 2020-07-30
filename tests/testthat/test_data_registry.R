context("Testing objects in the data registry")

test_that("all data products contain object components", {
  # Get all data products
  dps <- get_existing("data_product")

  # Extract object table uri for each data product and determine whether
  # that object contains an object component
  tmp <- lapply(seq_len(nrow(dps)), function(i) {
    data <- list(object = dps$object[i])
    out <- get_entry("object_component", clean_query(data))
    ifelse(is.null(out), F, T)
  }) %>% unlist() %>% which()

  # These data products have no component
  output <- dps[-tmp,]

  # Make sure output is empty
  assertthat::assert_that(nrow(output) == 0)
})
