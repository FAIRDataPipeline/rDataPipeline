#' rDataPipeline
#'
#' FAIR Data Pipeline API
#'
#' For more information see
#' [https://www.fairdatapipeline.org/](https://www.fairdatapipeline.org/)
#'
#' @name rDataPipeline-package
#' @aliases rDataPipeline
#' @docType package
#'
#' @rawNamespace import(assertthat, except = has_name)
#' @import cli
#' @import configr
#' @import dplyr
#' @importFrom git2r sha last_commit remote_url
#' @import httr
#' @import jsonlite
#' @importFrom openssl sha1
#' @import R6
#' @import rhdf5
#' @import semver
#' @importFrom stats setNames
#' @import usethis
#' @importFrom utils download.file read.csv unzip write.csv type.convert
#' @import yaml
#'
"_PACKAGE"