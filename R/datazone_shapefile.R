#' #' Data Zone Boundaries 2011
#' #'
#' #' Data zones are the key geography for the dissemination of small area
#' #' statistics in Scotland and are widely used across the public and private
#' #' sector. Composed of aggregates of Census Output Areas, data zones are large
#' #' enough that statistics can be presented accurately without fear of
#' #' disclosure and yet small enough that they can be used to represent
#' #' communities. They are designed to have roughly standard populations of
#' #' 500 to 1,000 household residents, nest within Local Authorities, have
#' #' compact shapes that respect physical boundaries where possible, and to
#' #' contain households with similar social characteristics. Aggregations of
#' #' data zones are often used to approximate a larger area of interest or a
#' #' higher level geography that statistics wouldn’t normally be available for.
#' #' Data zones also represent a relatively stable geography that can be used
#' #' to analyse change over time, with changes only occurring after a Census.
#' #' Following the update to data zones using 2011 Census data, there are now
#' #' 6,976 data zones covering the whole of Scotland.
#' #'
#' #' https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011
#' #'
#'
#' dir.create("tmpfiles")
#' dir.create(file.path("tmpfiles", "dzsf"))
#' download.file("http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip", "tmpfiles/dzsf/dzsf.zip")
#' unzip("tmpfiles/dzsf/dzsf.zip", exdir = "tmpfiles/dzsf")
#' file.remove("tmpfiles/dzsf/dzsf.zip")
