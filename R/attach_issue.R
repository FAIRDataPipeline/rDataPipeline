#' Attach issue to object
#'
#' This function is used to attach an issue to an object:
#' \itemize{
#' \item{an external object} - {specify the \code{external_object_doi} argument}
#' \item{a data product} - {specify the \code{namespace} and
#' \code{data_product} argument}
#' \item{a component within a data product} - {specify the \code{namespace},
#' \code{data_product}, and \code{component} argument}
#' }
#' See examples.
#'
#' @param description a \code{string} containing a free text description of the
#' \code{issue}
#' @param severity an \code{integer} specifying the severity of the \code{issue}
#' @param external_object_doi a \code{string} specifying the DOI or name of the
#' \code{external_object}
#' @param namespace a \code{string} specifying the name of the namespace
#' @param data_product a \code{string} specifying the name of the
#' \code{data_product}
#' @param component a \code{string} specifying the name of the
#' \code{object_component}, unique in the context of \code{object_component}
#' and its \code{object} reference
#' @param version a \code{string} specifying the version number of the
#' data_product or external_object
#' @param key API token from data.scrc.uk
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' # Attach issue to an external object
#' attach_issue(description = "An issue affecting an external object",
#'              severity = 10,
#'              external_object_doi = "Scottish spatial lookup table - dz",
#'              version = "0.1.0",
#'              key = key)
#'
#' # Attach issue to a data product
#' attach_issue(description = "An issue affecting a data product",
#'              severity = 10,
#'              namespace = "SCRC",
#'              data_product = "records/SARS-CoV-2/scotland/cases-and-management/carehomes",
#'              version = "0.20200916.0",
#'              key = key)
#'
#' # Attach issue to a component within a data product
#' attach_issue(description = "An issue affecting a component within a data product",
#'              severity = 10,
#'              namespace = "SCRC",
#'              data_product = "records/SARS-CoV-2/scotland/cases-and-management/carehomes",
#'              component = "date-country-response_rate",
#'              version = "0.20200916.0",
#'              key = key)
#' }}
#'
attach_issue <- function(description,
                         severity,
                         external_object_doi,
                         namespace,
                         data_product,
                         component,
                         version,
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

  if(missing(namespace) & missing(data_product) & missing(component)) {

    # Which external object do we want to associate with the issue?
    entries <- get_entry("external_object",
                         list(doi_or_unique_name = external_object_doi,
                              version = version))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    # Add this to the current list
    object_issues <- c(current_objects, objectId)
    component_issues <- current_components

    # Attach issue to data product --------------------------------------------

  } else if(missing(external_object_doi) &
            missing(component)) {

    # Which data product do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    entries <- get_entry("data_product", list(name = data_product,
                                              version = version,
                                              namespace = namespaceId))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    # Add this to the current list
    object_issues <- c(current_objects, objectId)
    component_issues <- current_components

    # Attach issue to object component ----------------------------------------

  } else if(missing(external_object_doi)) {

    # Which object component do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    entries <- get_entry("data_product", list(name = data_product,
                                              version = version,
                                              namespace = namespaceId))
    objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
    assertthat::assert_that(length(objectId) == 1)

    objectId <- clean_query(list(name = objectId))
    objectComponentId <- get_url("object_component", list(name = component,
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
