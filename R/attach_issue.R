#' Attach an issue to an object in the data registry
#'
#' This function is used to attach an issue to an object, which may be an
#' external object, a data product, or a component within a data product.
#'
#' @param description a \code{string} containing a free text description of the
#' \code{issue}
#' @param severity an \code{integer} specifying the severity of the \code{issue}
#' @param object a \code{list} comprising various elements depending on what
#' kind of object has an issue; see Details
#' @param key API token from data.scrc.uk
#'
#' @details The object argument should contain different elements depending on
#' what kind of object has an issue:
#' \itemize{
#' \item{an external object} - {\code{list(external_object_doi = "string",
#' version = "string")}}
#' \item{a data product} - {\code{list(data_product = "string",
#' namespace = "string", version = "string")}}
#' \item{a component within a data product} - {\code{list(data_product = "string",
#' namespace = "string", component = "string", version = "string")}}
#' }
#' where \code{external_object_doi} is a \code{string} specifying the DOI or
#' name of the \code{external_object}, \code{namespace} is a \code{string}
#' specifying the name of the namespace, \code{data_product} is a \code{string}
#' specifying the name of the \code{data_product}, \code{component} a
#' \code{string} specifying the name of the \code{object_component}, unique in
#' the context of \code{object_component} and its \code{object} reference, and
#' \code{version} is a \code{string} specifying the version number of the
#' \code{data_product} or \code{external_object}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Attach issue to an external object
#' object <- list(external_object_doi = "Scottish spatial lookup table - dz",
#'                version = "0.1.0")
#' attach_issue(description = "An issue affecting an external object",
#'              severity = 10,
#'              object = object,
#'              key = key)
#'
#'
#' # Attach issue to a data product
#' object <- list(data_product = "records/SARS-CoV-2/scotland/cases-and-management/carehomes",
#'                namespace = "SCRC",
#'                version = "0.20200916.0")
#' attach_issue(description = "An issue affecting a data product",
#'              severity = 10,
#'              object = object,
#'              key = key)
#'
#' # Attach issue to a component within a data product
#' object <- list(data_product = "records/SARS-CoV-2/scotland/cases-and-management/carehomes",
#'                namespace = "SCRC",
#'                component = "date-country-response_rate",
#'                version = "0.20200916.0")
#' attach_issue(description = "An issue affecting a component within a data product",
#'              severity = 10,
#'              object = object,
#'              key = key)
#' }
#'
attach_issue <- function(description,
                         severity,
                         object,
                         key) {

  # Does this issue already exist?
  if(check_exists("issue", list(description = description,
                                severity = severity))) {
    issueId <- get_url("issue", list(description = description,
                                     severity = severity))
    message("issue already exists")

  } else {
    # If not, create a new issue
    issueId <- new_issue(severity = severity,
                         description = description,
                         object_issues = list(),
                         component_issues = list(),
                         key = key)
  }

  # What does the issue look like?
  tmp <- get_entry("issue", list(description = description))[[1]]
  assertthat::assert_that(any(names(tmp) %in% "url"))

  # Which objects are currently associated with the issue?
  current_objects <- tmp$object_issues
  if(is.null(current_objects)) current_objects <- list()

  # Which object components are currently associated with the issue?
  current_components <- tmp$component_issues
  if(is.null(current_components)) current_components <- list()


  # Attach issue to external object -----------------------------------------

  if("external_object_doi" %in% names(object)) {

    if(!all(c("external_object_doi", "version") %in% names(object)))
      stop("object is incorrectly defined")

    # Which external object do we want to associate with the issue?
    entries <- get_entry("external_object",
                         list(doi_or_unique_name = object$external_object_doi,
                              version = object$version))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    # Add this to the current list
    object_issues <- c(current_objects, objectId)
    component_issues <- current_components

    # Attach issue to data product --------------------------------------------

  } else if("data_product" %in% names(object) &
            !"component" %in% names(object)) {

    if(!all(c("data_product", "namespace", "version") %in% names(object)))
      stop("object is incorrectly defined")

    # Which data product do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = object$namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    entries <- get_entry("data_product", list(name = object$data_product,
                                              version = object$version,
                                              namespace = namespaceId))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    # Add this to the current list
    object_issues <- c(current_objects, objectId)
    component_issues <- current_components

    # Attach issue to object component ----------------------------------------

  } else if("data_product" %in% names(object) &
            "component" %in% names(object)) {

    if(!all(c("data_product", "namespace", "component", "version") %in%
            names(object))) stop("object is incorrectly defined")

    # Which object component do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = object$namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    entries <- get_entry("data_product", list(name = object$data_product,
                                              version = object$version,
                                              namespace = namespaceId))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    objectId <- clean_query(list(name = objectId))
    objectComponentId <- get_url("object_component", list(name = object$component,
                                                          object = objectId))

    # Add this to the current list
    object_issues <- current_objects
    component_issues <- c(current_components, objectComponentId)

  } else
    stop("Please check your arguments have been inputted correctly")


  # Upload issue to the data registry ---------------------------------------

  message("Attaching issue to object component")
  patch_data(url = issueId,
             data = list(severity = severity,
                         description = description,
                         object_issues = current_objects,
                         component_issues = component_issues),
             key = key)
}
