#' get_provenance
#'
#' @param data_product data_product
#' @param version version
#' @param namespace namespace
#'
#' @export
#'
get_provenance <- function(data_product, version, namespace) {

  # Get provenance URL
  namespace_entry <- get_entry("namespace", list(name = namespace))
  assertthat::assert_that(length(namespace_entry) == 1)
  namespace_url <- namespace_entry[[1]]$url

  dp_entry <- get_entry("data_product",
                        list(name = data_product,
                             version = version,
                             namespace = extract_id(namespace_url)))
  assertthat::assert_that(length(dp_entry) == 1)
  prov_url <- dp_entry[[1]]$prov_report
  api_url <- paste0(prov_url, "?format=svg")

  # Get XML
  svg <- httr::GET(api_url) %>%
    httr::content(as = "text", encoding = "UTF-8")
  xml <- XML::xmlParse(svg, asText = TRUE)
  assertthat::assert_that(all(class(xml) %in% c("XMLInternalDocument",
                                                "XMLAbstractDocument")))
  xml_file <- tempfile(fileext = ".xml")
  XML::saveXML(xml, xml_file)

  # Generate and display png
  png_file <- tempfile(fileext = ".png")
  rsvg::rsvg_png(xml_file, png_file)
  display_png(png_file)
}

display_png <- function(img) {
  img <- png::readPNG(img)
  plot.new()
  plot.window(0:1, 0:1, asp = 1)
  rasterImage(img, 0, 0, 1, 1)
}
