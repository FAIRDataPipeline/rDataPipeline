#' register_everything
#'
#' This function assumes that (1) the source data is being saved as a
#' \code{csv} file, (2) the data product is being saved as an \code{h5} file,
#' (3) the source data and data product are being stored on the boydorr ftp
#' server (4) the submission script is being stored in the
#' ScottishCovidResponse/SCRCdata respository
#'
#' @param product_name *e.g.*
#' @param version_number *e.g.*
#' @param doi_or_unique_name *e.g.*
#' @param namespace a \code{string} specifying the namespace; by default this
#' is "SCRC"
#' @param submission_script a \code{string} specifying the filename of the
#' submission script located in \code{inst/namespace/} *e.g.* "nrs_demographics.R"
#' @param original_source_name *e.g.*
#' @param original_sourceId *e.g.*
#' @param original_root *e.g.* "https://www.nrscotland.gov.uk"
#' @param original_path *e.g.*
#' @param key API token from data.scrc.uk
#'
#' @export
#'
register_everything <- function(product_name,
                                version_number,
                                doi_or_unique_name,
                                namespace,
                                submission_script,
                                original_source_name,
                                original_sourceId,
                                original_root,
                                original_path,
                                key) {

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
    root = "https://github.com",
    key = key)

  # where is the source data downloaded to locally? -------------------------
  local_path <- file.path("data-raw", product_path)
  source_filename <- paste0(version_number, ".csv")

  original_storageRootId <- new_storage_root(
    name = original_source_name,
    root = original_root,
    key = key)

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
  sourceDataURIs <- upload_source_data(
    doi_or_unique_name = doi_or_unique_name,
    original_source_id = original_sourceId, #
    original_root_id = original_storageRootId, #
    original_path = original_path, #
    local_path = file.path(local_path, source_filename),
    storage_root_id = source_storageRootId,
    target_path = paste(product_name, source_filename, sep = "/"),
    download_date = todays_date,
    version = version_number,
    key = key)

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

  upload_object_links(run_date = todays_date,
                      description = paste("Script run to upload and process",
                                          doi_or_unique_name),
                      code_repo_id = githubRepoURIs$repo_objectId,
                      submission_script_id = submissionScriptURIs$script_objectId,
                      inputs = list(sourceDataURIs$source_objectComponentId),
                      outputs = dataProductURIs$product_objectComponentId,
                      key = key)
}
