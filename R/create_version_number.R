#' Create version number
#'
#' Creates a version number from either *a date and a version*
#' **or** *a date and major and patch*
#' **or** *major minor patch*.
#' If no parameters are supplied a default version is returned
#' \code{0.1.0}
#' This function prioritizes download date and version over all other parameters
#'
#' @param download_date (Optional) download_date
#' This can either be a date or datetime
#' but must include the full year
#' *e.g* from
#' \code{Sys.Date()
#' 2020-01-01}
#' Or from
#' \code{Sys.time()
#' 2020-01-01 00:00:00 BST}
#' note: also accepts / delimited dates
#' *e.g* \code{01/02/2020}
#' or \code{2020/01/01}
#' and accepts date without delimiters but assumes ddmmyyyy or yyyymmdd
#' *e.g.* \code{20200201}
#' @param version version number using major.minor.patch numbering
#' *e.g.* \code{0.1.0}
#' or major.patch
#' *e.g.* \code{0.0}
#' @param major major number if not using \code{version}
#' @param minor minor number if not using \code{date}
#' @param patch patch number if not using \code{version}
#'
#' @return returns a character vector in the format of \code{major.minor.patch}
#' *e.g.* \code{0.20200101.0}
#'
#' @family create functions
#'
create_version_number <- function(download_date = NULL,
                                  version = NULL,
                                  major = 0,
                                  minor = "1",
                                  patch = 0) {

  # Check if a version number was used
  if (!is.null(version)) {
    # Check if version is major.minor.patch
    if (grepl("[0-9]+[`.`][0-9]+[`.`][0-9]+", version)) {
      patch <- strsplit(version, "[`.`]")[[1]][3]
      major <- strsplit(version, "[`.`]")[[1]][1]
    }
    else if (grepl("[0-9]+[`.`][0-9]", version)) {
      patch <- strsplit(version, "[`.`]")[[1]][2]
      major <- strsplit(version, "[`.`]")[[1]][1]
    }
    else if (grepl("^[0-9]+$", version) & (!grepl("[.]", version)))
      major <- version
    else
      stop("Version must be major.patch or major.minor.patch or major")
  }

  # check if a download date was supplied
  if (!is.null(download_date)) {
    # Remove time
    tmp <- gsub(" [0-9]*:[0-9]*:[0-9]*$", "", download_date)
    # Allow YYYYMMDD and allow DDMMYYYY but only if the year contains 20xx
    # or 19xx, warn if DDMMYYYY as this will be reversed
    if (grepl("^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$", tmp)) {
      if (grepl("^19+", tmp) | grepl("^20+", tmp))
        minor <- tmp
      else if (grepl("+20[0-9][0-9]$", tmp) | grepl("+20[0-9][0-9]$", tmp)) {
        warning("assuming yyyymmdd format")
        minor <- paste0(strsplit(tmp, NULL)[[1]][c(5, 6, 7, 8, 3, 4, 1, 2)],
                        collapse = "")
      }
      else
        stop("Invalid Date Format: Year should be 2019 or 202x")
      }
    # Do not allow DDMMYY or YYMMDD as no way to tell which way round the
    # date is
    else if (grepl("^[0-9][0-9].[0-9][0-9].[0-9][0-9]$", tmp))
      stop("Invalid Date format: Please use full year")

    #  reverse date if dd/mm/yyyy or dd-mm-yyyy
    else if (grepl("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", tmp))
      minor <- paste(strsplit(gsub("[^0-9]", "/", tmp), "/")[[1]][c(3, 2, 1)],
                     collapse = "/") #  reverse the date
    else if (grepl("[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]", tmp))
      minor <- tmp
    else
      stop("Invalid date format")
    # Remove non numerical characters
    minor <- gsub("[^a-zA-Z0-9]", "", minor)
  }

  return(paste0(major, ".", minor, ".", patch))

}
