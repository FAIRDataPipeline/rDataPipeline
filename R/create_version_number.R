#' create_version_number
#'
#' @param download_date download_date
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
#' @param major major number if not using \code{version}
#' @param minor minor number if not using \code{date}
#' @param patch patch number if not using \code{version}
#'
#' @return returns a character vector in the format of \code{YYYYMMDD.major.minor.patch}
#' *e.g.* \code{20200101.0.1.0}
#'
#' @export
#'
create_version_number <- function(download_date = NULL,
                                  version = NULL, major = NULL, minor = "1", patch = NULL) {
  if(is.null(major))
    major <- 0
  if(is.null(patch))
    patch <- 0
  if(!is.null(version))
  {
    # Check if version is major.minor.patch
    if(grepl("[0-9]+[`.`][0-9]+[`.`][0-9]+", version)){
      patch <- strsplit(version, "[`.`]")[[1]][3]
      major <- strsplit(version, "[`.`]")[[1]][1]
    }
    else if(grepl("[0-9]+[`.`][0-9]", version))
    {
      patch <- strsplit(version, "[`.`]")[[1]][2]
      major <- strsplit(version, "[`.`]")[[1]][1]
    }
    else if(grepl("^[0-9]+$", version) & (!grepl("[.]", version)))
      major <- version
    else
      stop("Version must be major.patch or major.minor.patch or major")
  }
  if(!is.null(download_date)){
    # Remove time
    tmp <- gsub(" [0-9]*:[0-9]*:[0-9]*$", "", download_date)
    # allow 20200101 format but warn if yyyyddmm
    if(grepl("^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$", tmp)){
      if(grepl("^19+", tmp) | grepl("^20+", tmp))
        minor <- tmp
      else if(grepl("+20[0-9][0-9]$", tmp) | grepl("+20[0-9][0-9]$", tmp)){
        warning("assuming yyyymmdd format")
        minor <- paste0(strsplit(tmp, NULL)[[1]][c(5,6,7,8,3,4,1,2)], collapse = "")
      }
      else
        stop("Invalid Date Format: Year should be 2019 or 202x")}

    else if(grepl("^[0-9][0-9].[0-9][0-9].[0-9][0-9]$", tmp))
      stop("Invalid Date format: Please use full year")

    #  reverse date if dd/mm/yyyy or dd-mm-yyyy
    else if(grepl("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", tmp))
      minor <- paste(strsplit(gsub("[^0-9]", "/", tmp), "/")[[1]][c(3,2,1)], collapse = "/") #  reverse the date
    else if(grepl("[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]", tmp))
      minor <- tmp
    else
      stop("Invalid date format")
    minor <- gsub("-", "", minor) %>% gsub("/", "", .)
  }

  return(paste0(major, ".", minor, ".", patch))

}
