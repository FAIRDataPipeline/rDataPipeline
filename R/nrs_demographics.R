#' #' Small Area Population Estimates 2018
#' #'
#' #' https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series
#' #'
#'
#' download.file("")
#'
#' dir.create("tmpfiles")
#' dir.create(file.path("tmpfiles", "dzdemog"))
#' download.file("https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/persons/sape-2018-persons.xlsx", "tmpfiles/dzdemog/dzsf.xlsx")
#' file.remove("tmpfiles/dzsf/dzsf.zip")
