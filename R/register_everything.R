#' register_everything
#'
#' This function assumes that (1) the source data is being saved as a
#' \code{csv} file, (2) the data product is being saved as an \code{h5} file,
#' (3) the source data and data product are being stored on the boydorr ftp
#' server (4) the submission script is being stored in the
#' ScottishCovidResponse/SCRCdata respository. Data products may be generated
#' from a single or multiple original sources, see Examples below for details.
#'
#' @param product_name a \code{string} specifying the product_name (used to
#' identify the data product as well as being used to generate various file
#' locations)
#' @param version_number a \code{string} specifying the version identifier of
#' the \code{data_product} (must conform to semantic versioning syntax)
#' @param save_location (optional) a \code{string}, which when prepended to
#' product_name, will specify where to download the original source to and
#' where to save the processed data product to. Default is "data-raw"
#' @param doi_or_unique_name a \code{string} specifying the DOI or name of the
#' \code{external_object} (source data)
#' @param namespace a \code{string} specifying the namespace; by default this
#' is "SCRC"
#' @param submission_script a \code{string} specifying the filename of the
#' submission script located in the \code{inst/namespace/} directory of the
#' SCRCdata package
#' @param original_source_name a \code{string} specifying the original source
#' name
#' @param original_sourceId a \code{string} specifying the API URL of the
#' associated \code{source} table
#' @param original_root a \code{string} specifying the root of the original
#' source (must have a trailing slash), which when prepended to
#' \code{original_path} produces a complete URL
#' @param original_path a \code{string} specifying the path from the
#' original_root, which when appended to \code{original_root} produces a
#' complete URL
#' @param source_filename a \code{string} specifying the source filename
#' @param accessibility (optional) an \code{integer} value for the accessibility
#' enum associated with \code{original_root}, where 0 is public (default) and
#' 1 is private
#' @param key API token from data.scrc.uk
#'
#' @export
#'
#' @details Note that when a data product is generated from multiple original sources,
#' \code{original_source_name}, \code{original_sourceId}, \code{original_root},
#' and \code{original_path} are input as named lists (see Example below).
#'
#' @examples
#' \dontrun{
#' # A single original source
#' register_everything(product_name = "geography/scotland/lookup_table",
#' version_number = "0.1.0",
#' save_location = "data-raw",
#' doi_or_unique_name = "Scottish spatial lookup table",
#' namespace = "SCRC",
#' submission_script = "scotgov_dz_lookup.R",
#' original_source_name = "Scottish Government",
#' original_sourceId = "https://data.scrc.uk/api/source/3932/",
#' original_root = "https://www.gov.scot/",
#' original_path = "path/somefile.csv",
#' source_filename = "paste0(version_number, ".csv")",
#' accessibility = 0,
#' key)
#'
#' # Multiple original sources
#' register_everything(product_name = "geography/scotland/lookup_table",
#' version_number = "0.1.0",
#' save_location = "data-raw",
#' doi_or_unique_name = "Scottish spatial lookup table",
#' namespace = "SCRC",
#' submission_script = "scotgov_dz_lookup.R",
#' original_source_name = list(simd = "Scottish Government",
#' dz = "Scottish Government Open Data Repository downloadable file"),
#' original_sourceId = list(simd = "https://data.scrc.uk/api/source/3932/",
#' dz = "https://data.scrc.uk/api/source/3976/"),
#' original_root = list(simd = "https://www.gov.scot/", dz = "http://statistics.gov.scot/"),
#' original_path = list(simd = "path/thisfile.csv", dz = "downloads/anotherfile.csv"),
#' source_filename = list(simd = paste0(version_number, ".xlsx"),
#' dz = paste0(version_number, ".csv")),
#' accessibility = 0,
#' key)
#' }
#'
register_everything <- function(product_name,
                                version_number,
                                save_location = "data-raw",
                                doi_or_unique_name,
                                namespace,
                                submission_script,
                                original_source_name,
                                original_sourceId,
                                original_root,
                                original_path,
                                source_filename,
                                accessibility = 0,
                                key) {

  # If any of the original_* arguments is a list (presumably containing
  # multiple entries, do some checks...
  if(is.list(original_source_name) | is.list(original_sourceId) |
     is.list(original_root) | is.list(original_path) |
     is.list(source_filename)) {

    # Are they all lists?
    if(!is.list(original_source_name) | !is.list(original_sourceId) |
       !is.list(original_root) | !is.list(original_path)|
       !is.list(source_filename))
      stop(paste("If one of the arguments original_source_name,",
                 "original_sourceId, original_root, original_path, and",
                 "source_filename is a list (implying multiple original",
                 "sources), they should all be lists"))

    # Are they all the same length?
    number_of_elements <- c(length(original_source_name),
                            length(original_sourceId),
                            length(original_root),
                            length(original_path),
                            length(source_filename))

    if(length(unique(number_of_elements)) != 1)
      stop(paste("If original_source_name, original_sourceId, original_root,",
                 "and original_path are lists (implying multiple original",
                 "sources), they should all be the same length"))

    # Do their elements have the same names (in the same order)?
    check1 <- all(names(original_source_name) != names(original_sourceId))
    check2 <- all(names(original_source_name) != names(original_root))
    check3 <- all(names(original_source_name) != names(original_path))
    check4 <- all(names(original_source_name) != names(source_filename))

    if(any(check1, check2, check3, check4))
      stop(paste("If original_source_name, original_sourceId, original_root,",
                 "original_path, and source_filename are lists (implying",
                 "multiple original sources), their elements should have the",
                 "same names"))
  }


  # initialise objects ------------------------------------------------------

  todays_date <- Sys.time()
  product_path <- do.call(file.path, as.list(strsplit(product_name, "/")[[1]]))

  # namespace ---------------------------------------------------------------
  namespaceId <- new_namespace(name = namespace,
                               key = key)

  # ensure that github is in the data registry ------------------------------
  github_info <- get_package_info(repo = "ScottishCovidResponse/SCRCdata",
                                  script_path = paste0("inst/SCRC/",
                                                       submission_script),
                                  package = "SCRCdata")

  repo_storageRootId <- new_storage_root(
    name = paste0(github_info$repo_storageRoot),
    root = "https://github.com/",
    key = key)

  # where is the source data downloaded to locally? -------------------------
  local_path <- file.path(save_location, product_path)

  # If original_* arguments contain multiple entries...
  if(is.list(original_source_name)) {
    original_storageRootId <- lapply(
      seq_along(original_source_name), function(x) {
        new_storage_root(name = original_source_name[[x]],
                         root = original_root[[x]],
                         accessibility = accessibility,
                         key = key)
      })
  } else {
    original_storageRootId <- new_storage_root(
      name = original_source_name,
      root = original_root,
      accessibility = accessibility,
      key = key)
  }

  # where is the data product saved to locally? -----------------------------
  processed_path <- file.path(save_location, product_path)
  product_filename <- paste0(version_number, ".h5")

  # where is the source data stored? ----------------------------------------
  source_storageRootId <- new_storage_root(
    name = "boydorr",
    root = "ftp://boydorr.gla.ac.uk/scrc/",
    key = key)

  # where is the data product stored? ---------------------------------------
  product_storageRootId <- new_storage_root(
    name = "boydorr",
    root = "ftp://boydorr.gla.ac.uk/scrc/",
    key = key)

  # where is the submission script stored? ----------------------------------
  script_storageRoot <- "text_file"
  submission_text <- paste("R -f", github_info$submission_script)

  script_storageRootId <- new_storage_root(
    name = script_storageRoot,
    root = "https://data.scrc.uk/api/text_file/",
    key = key)

  # upload source metadata to registry --------------------------------------

  # If original_* arguments contain multiple entries...
  if(is.list(original_source_name)) {

    primary <- rep(FALSE, length(original_source_name))
    primary[1] <- TRUE

    sourceDataURIs <- lapply(seq_along(original_source_name), function(x) {
      upload_source_data(
        doi_or_unique_name = paste(doi_or_unique_name, names(original_root)[x],
                                   sep = " - "),
        original_source_id = original_sourceId[[x]], # list
        original_root_id = original_storageRootId[[x]], # list
        original_path = original_path[[x]], # list
        primary_not_supplement = primary[x],
        local_path = file.path(local_path, names(original_root)[x],
                               source_filename[[x]]),
        storage_root_id = source_storageRootId,
        target_path = paste(product_name, names(original_root)[x],
                            source_filename[[x]], sep = "/"),
        download_date = todays_date,
        version = version_number,
        key = key)
    })
  } else {
    sourceDataURIs <- upload_source_data(
      doi_or_unique_name = doi_or_unique_name,
      original_source_id = original_sourceId,
      original_root_id = original_storageRootId,
      original_path = original_path,
      primary_not_supplement = TRUE,
      local_path = file.path(local_path, source_filename),
      storage_root_id = source_storageRootId,
      target_path = paste(product_name, source_filename, sep = "/"),
      download_date = todays_date,
      version = version_number,
      key = key)
  }

  # upload data product metadata to the registry ----------------------------
  dataProductURIs <- upload_data_product(
    storage_root_id = product_storageRootId,
    name = product_name,
    processed_path = file.path(processed_path, product_filename),
    product_path = paste(product_name, product_filename, sep = "/"),
    version = version_number,
    namespace_id = namespaceId,
    key = key)

  # upload submission script metadata to the registry -----------------------
  submissionScriptURIs <- upload_submission_script(
    storage_root_id = script_storageRootId,
    hash = openssl::sha1(submission_text),
    text = submission_text,
    run_date = todays_date,
    key = key)

  # link objects together ---------------------------------------------------
  githubRepoURIs <- upload_github_repo(
    storage_root_id = repo_storageRootId,
    repo = github_info$script_gitRepo,
    hash = github_info$github_hash,
    version = github_info$repo_version,
    key = key)

  # If original_* arguments contain multiple entries...
  if(is.list(original_source_name)) {
    inputs <- lapply(sourceDataURIs, function(x) x$source_objectComponentId)
  } else {
    inputs <- list(sourceDataURIs$source_objectComponentId)
  }

  upload_object_links(run_date = todays_date,
                      description = paste("Script run to upload and process",
                                          doi_or_unique_name),
                      code_repo_id = githubRepoURIs$repo_objectId,
                      submission_script_id = submissionScriptURIs$script_objectId,
                      inputs = inputs,
                      outputs = dataProductURIs$product_objectComponentId,
                      key = key)
}
