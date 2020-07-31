#' attach_issue
#'
#' This function is used to attach an issue to either
#' * an external object (in which case you must specify the
#' \code{external_object} argument)
#' * a data product (in which case you must specify the \code{namespace} and
#' \code{data_product} arguments), or
#' * a component within a data product (in which case you must specify the
#' \code{namespace}, \code{data_product}, and \code{component} arguments)
#'
#' @param external_object *e.g.*
#' "scottish coronavirus-covid-19-management-information"
#' @param namespace *e.g.* "SCRC"
#' @param data_product *e.g.* "records/SARS-CoV-2/scotland/cases_and_management"
#' @param component *e.g.* "testing_location/date-cumulative"
#' @param description *e.g.* "Data dump caused a spike on the 15th of June"
#' @param severity *e.g.* 19
#' @param key key
#'
#' @export
#'
attach_issue <- function(external_object,
                         namespace,
                         data_product,
                         component,
                         description,
                         severity,
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


  if(!missing(external_object)) {

    # Attach issue to external object -----------------------------------------

    if(!missing(namespace) | !missing(data_product) | !missing(components))
      stop(paste("Attaching an issue to an external object does not require",
                 "the namespace, data_product, or component arguments"))

    # Which objects are currently associated with the issue?
    current_objects <- tmp$object_issues

    # Which external object do we want to associate with the issue?
    objectId <- get_entry("external_object",
                          list(doi_or_unique_name = external_object))$object

    # Add this to the current list
    object_issues <- c(current_objects, objectId)

    # Attach issue to external object in the data registry
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = object_issues,
                           component_issues = tmp$component_issues),
               key = key)



  } else if(!missing(namespace) &
            !missing(data_product) &
            missing(component)) {

    # Attach issue to data product --------------------------------------------

    if(!missing(external_object))
      stop(paste("Attaching an issue to a data product does not require",
                 "the component or external_object arguments"))

    # Which objects are currently associated with the issue?
    current_objects <- tmp$object_issues

    # Which data product do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    objectId <- get_entry("data_product", list(name = data_product,
                                               namespace = namespaceId))$object

    # Add this to the current list
    object_issues <- c(current_objects, objectId)

    # Attach issue to external object in the data registry
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = object_issues,
                           component_issues = tmp$component_issues),
               key = key)



  } else if(!missing(namespace) &
            !missing(data_product) &
            !missing(component)) {

    # Attach issue to object components ---------------------------------------

    if(!missing(external_object))
      stop(paste("Attaching an issue to an object component does not require",
                 "the external_object argument"))

    # Which object components are currently associated with the issue?
    current_components <- tmp$component_issues

    # Which object component do we want to associate with the issue?
    namespaceId <- get_url("namespace", list(name = namespace))
    namespaceId <- clean_query(list(name = namespaceId))
    objectId <- get_entry("data_product", list(name = data_product,
                                               namespace = namespaceId))$object
    objectId <- clean_query(list(name = objectId))
    objectComponentId <- get_entry("object_component", list(name = component,
                                                            object = objectId))$object

    # Add this to the current list
    component_issues <- c(current_components, objectComponentId)

    # Attach issue to external object in the data registry
    patch_data(url = issueId,
               data = list(severity = severity,
                           description = description,
                           object_issues = tmp$object_issues,
                           component_issues = component_issues),
               key = key)
  }
}
