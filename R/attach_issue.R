#' attach_issue
#'
#' This function is used to attach an issue to either
#' * an external object (in which case you must specify the
#' \code{external_object_doi} argument)
#' * a data product (in which case you must specify the \code{namespace} and
#' \code{data_product} arguments), or
#' * a component within a data product (in which case you must specify the
#' \code{namespace}, \code{data_product}, and \code{component} arguments)
#'
#' @param description a \code{string} containing a free text description of the
#' issue
#' @param severity a \code{numeric} object specifying the severity of the issue
#' @param external_object_doi a \code{string} specifying the external_object_doi
#' (used when issue is to be attached to an external object)
#' @param namespace a \code{string} specifying the namespace
#' @param data_product a \code{string} specifying the name of the  data_product
#' @param component a \code{string} specifying the name of the object_component
#' component
#' @param key API token from data.scrc.uk
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' attach_issue(description = "Data dump caused a spike on the 15th of June",
#' severity = 19,
#' external_object_doi = "scottish coronavirus-covid-19-management-information",
#' namespace = "SCRC",
#' data_product = "records/SARS-CoV-2/scotland/cases_and_management",
#' component = "testing_location/date-cumulative",
#' key = key)
#' }}
#'
attach_issue <- function(description,
                         severity,
                         external_object_doi,
                         namespace,
                         data_product,
                         component,
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
  tmp <- get_entry("issue", list(description = description))


  # Attach issue to external object -----------------------------------------
  if(missing(namespace) & missing(data_product) & missing(component)) {

    # Which objects are currently associated with the issue?
    current_objects <- tmp$object_issues

    # Which external object do we want to associate with the issue?
    objectId <- get_entry("external_object",
                          list(doi_or_unique_name = external_object_doi))$object

    # Add this to the current list
    object_issues <- c(current_objects, objectId)

    # Attach issue to external object in the data registry
    message("Attaching issue to external object")
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = object_issues,
                           component_issues = tmp$component_issues),
               key = key)



  } else if(missing(external_object_doi) &
            missing(component)) {

    # Attach issue to data product --------------------------------------------

    # Which objects are currently associated with the issue?
    current_objects <- tmp$object_issues

    # Which data product do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    objectId <- get_entry("data_product", list(name = data_product,
                                               namespace = namespaceId))$object

    # Add this to the current list
    object_issues <- c(current_objects, objectId)

    # Attach issue to data product in the data registry
    message("Attaching issue to data product")
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = object_issues,
                           component_issues = tmp$component_issues),
               key = key)



  } else if(missing(external_object_doi)) {

    # Attach issue to object component ---------------------------------------

    # Which object components are currently associated with the issue?
    current_components <- tmp$component_issues

    # Which object component do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    objectId <- get_entry("data_product", list(name = data_product,
                                               namespace = namespaceId))$object
    objectId <- clean_query(list(name = objectId))
    objectComponentId <- get_url("object_component", list(name = component,
                                                          object = objectId))

    # Add this to the current list
    component_issues <- c(current_components, objectComponentId)

    # Attach issue to object component in the data registry
    message("Attaching issue to object component")
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = tmp$object_issues,
                           component_issues = component_issues),
               key = key)
  } else
    stop("Please check your arguments have been inputted correctly")
}
