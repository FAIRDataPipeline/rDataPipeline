#' register_everything_multiple_sources
#'
#' This function assumes that (1) the source data is being saved as a
#' \code{csv} file, (2) the data product is being saved as an \code{h5} file,
#' (3) the source data and data product are being stored on the boydorr ftp
#' server (4) the submission script is being stored in the
#' ScottishCovidResponse/SCRCdata respository
#'
#' @param product_name *e.g.* "geography/scotland/lookup_table"
#' @param version_number *e.g.* "0.1.0"
#' @param doi_or_unique_name *e.g.* "Scottish spatial lookup table"
#' @param namespace a \code{string} specifying the namespace; by default this
#' is "SCRC"
#' @param submission_script a \code{string} specifying the filename of the
#' submission script located in \code{inst/namespace/} *e.g.* "scotgov_dz_lookup.R"
#' @param original_source_name *e.g.* list(simd = "Scottish Government", dz = "Scottish Government Open Data Repository downloadable file")
#' @param original_sourceId *e.g.* list(simd = "https://data.scrc.uk/api/source/3932/", dz = "https://data.scrc.uk/api/source/3976/")
#' @param original_root *e.g.* list(simd = "https://www.gov.scot/", dz = "http://statistics.gov.scot/")
#' @param original_path *e.g.* list(simd = "binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bdatazone%2Blookup.xlsx?forceDownload=true", dz = "downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv")
#' @param accessibility 0
#' @param key API token from data.scrc.uk
#'
#' @export
#'
register_everything_multiple_sources <- function(product_name,
                                                 version_number,
                                                 doi_or_unique_name,
                                                 namespace,
                                                 submission_script,
                                                 original_source_name,
                                                 original_sourceId,
                                                 original_root,
                                                 original_path,
                                                 accessibility = 0,
                                                 key) {

  # If any of the original_* arguments is a list (presumably containing
  # multiple entries, do some checks...
  if(is.list(original_source_name) | is.list(original_sourceId) |
     is.list(original_root) | is.list(original_path)) {

    # Are they all lists?
    if(!is.list(original_source_name) | !is.list(original_sourceId) |
       !is.list(original_root) | !is.list(original_path))
      stop(paste("If one of the arguments original_source_name,",
                 "original_sourceId, original_root, and original_path is a",
                 "list (implying multiple original sources), they should all",
                 "be lists"))

    # Are they all the same length?
    number_of_elements <- c(length(original_source_name),
                            length(original_sourceId),
                            length(original_root),
                            length(original_path))

    if(length(unique(number_of_elements)) != 1)
      stop(paste("If original_source_name, original_sourceId, original_root,",
                 "and original_path are lists (implying multiple original",
                 "sources), they should all be the same length"))

    # Do their elements have the same names (in the same order)?
    check1 <- all(names(original_source_name) != names(original_sourceId))
    check2 <- all(names(original_source_name) != names(original_root))
    check3 <- all(names(original_source_name) != names(original_path))

    if(any(check1, check2, check3))
      stop(paste("If original_source_name, original_sourceId, original_root,",
                 "and original_path are lists (implying multiple original",
                 "sources), their elements should have the same names"))
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
    name = github_info$repo_storageRoot,
    root = "https://github.com/",
    key = key)

  # where is the source data downloaded to locally? -------------------------
  local_path <- file.path("data-raw", product_path)
  source_filename <- paste0(version_number, ".csv")

  # If original_* arguments contain multiple entries...
  if(is.list(original_source_name)) {
    original_storageRootId <- lapply(
      seq_along(original_source_name), function(x) {
        new_storage_root(name = original_source_name[[x]],
                         root = original_root[[x]],
                         accessibility = accessibility,
                         key = key)
      })
  }

  # where is the data product saved to locally? -----------------------------
  processed_path <- file.path("data-raw", product_path)
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
                               source_filename),
        storage_root_id = source_storageRootId,
        target_path = paste(product_name, names(original_root)[x],
                            source_filename, sep = "/"),
        download_date = todays_date,
        version = version_number,
        key = key)
    })
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

  inputs <- lapply(sourceDataURIs, function(x) x$source_objectComponentId)

  upload_object_links(run_date = todays_date,
                      description = paste("Script run to upload and process",
                                          doi_or_unique_name),
                      code_repo_id = githubRepoURIs$repo_objectId,
                      submission_script_id = submissionScriptURIs$script_objectId,
                      inputs = inputs,
                      outputs = dataProductURIs$product_objectComponentId,
                      key = key)
}
