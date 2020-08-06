##################################################################
##        table definitions and functions for validation        ##
##             table definitions updated 05/08/2020             ##
##################################################################

table_list <- c("users",
            "groups",
            "issue",
            "object",
            "object_component",
            "code_run",
            "storage_root",
            "storage_location",
            "source",
            "external_object",
            "quality_controlled",
            "keyword",
            "author",
            "licence",
            "namespace",
            "data_product",
            "code_repo_release",
            "key_value",
            "text_file")



# Users
users.writable <- c()
users.readable <- c("url",
                   "username",
                   "full_name",
                   "email",
                   "orgs",
                   users.writable)
users.queryable <- c("username")
users.optional <- c()

groups.writable <- c()
groups.readable <- c()
groups.queryable <- c()
groups.optional <- c()

issue.writable <- c("severity",
                    "description",
                    "object_issues",
                    "component_issues")
issue.readable <- c("url",
                    "last_updated",
                    "updated_by",
                    issue.writable)
issue.queryable <- c("object_issues",
                     "component_issues",
                     "updated_by",
                     "last_updated",
                     "severity",
                     "description",
                     "cursor",
                     "format")
issue.optional <- c()

object.writable <- c("description",
                     "storage_location",
                     "issues")
object.readable <- c("url",
                     "last_updated",
                     "updated_by",
                     "components",
                     "data_product",
                     "code_repo_release",
                     "external_object",
                     "quality_control",
                     "authors",
                     "licences",
                     "keywords",
                     object.writable)
object.queryable <- c("components",
                      "code_repo_of",
                      "config_of",
                      "submission_script_of",
                      "external_object",
                      "quality_control",
                      "keywords",
                      "authors",
                      "licences",
                      "data_product",
                      "code_repo_release",
                      "metadata",
                      "updated_by",
                      "last_updated",
                      "storage_location",
                      "description",
                      "issues",
                      "cursor",
                      "format")
object.optional <- c("description",
                     "issues",
                     "storage_location")

object_component.writable <- c("object",
                               "name",
                               "description",
                               "issues")
object_component.readable <- c("url",
                               "last_updated",
                               "updated_by",
                               "input_of",
                               "output_of",
                               object_component.writable)
object_component.queryable <- c("inputs_of",
                                "outputs_of",
                                "updated_by",
                                "last_updated",
                                "object",
                                "name",
                                "description",
                                "issues",
                                "cursor",
                                "format")
object_component.optional <- c("description",
                               "issues")

code_run.writable <- c("run_date",
                       "description",
                       "code_repo",
                       "model_config",
                       "submission_script",
                       "inputs",
                       "outputs")
code_run.readable <- c("url",
                       "last_updated",
                       "updated_by",
                       code_run.writable,
                       "prov_report")
code_run.queryable <- c("updated_by",
                        "last_updated",
                        "code_repo",
                        "model_config",
                        "submission_script",
                        "run_date",
                        "description",
                        "inputs",
                        "outputs",
                        "cursor",
                        "format")
code_run.optional <- c("description",
                       "code_repo")

storage_root.writable <- c("name",
                           "root",
                           "path",
                           "accessability")
storage_root.readable <- c("url",
                           "last_updated",
                           "updated_by",
                           storage_root.writable)
storage_root.queryable <- c("locations",
                            "updated_by",
                            "last_updated",
                            "name",
                            "root",
                            "accessibility",
                            "cursor",
                            "format")
storage_root.optional <- c("accessability")

storage_location.writable <- c("path",
                               "hash",
                               "store_root")
storage_location.readable <- c("url",
                               "last_updated",
                               "updated_by",
                               storage_location.writable)
storage_location.queryable <- c("location_for_object",
                                "original_store_of",
                                "updated_by",
                                "last_updated",
                                "path",
                                "hash",
                                "storage_root",
                                "cursor",
                                "format")
storage_location.optional <- c()

source.writable <- c("name",
                "abbreviation",
                "website")
source.readable <- c("url",
                "last_updated",
                "updated_by",
                source.writable)
source.queryable <- c("external_objects",
                      "updated_by",
                      "last_updated",
                      "name",
                      "abbreviation",
                      "website",
                      "cursor",
                      "format")
source.optional <- c("website")

external_object.writable <- c("doi_or_unique_name",
                              "primary_not_supplement",
                              "release_date",
                              "title",
                              "description",
                              "version",
                              "object",
                              "source",
                              "original_store")
external_object.readable <- c("url",
                              "last_updated",
                              "updated_by",
                              external_object.writable)
external_object.queryable <- c("updated_by", "last_updated",
                               "object",
                               "doi_or_unique_name",
                               "primary_not_supplement",
                               "release_date",
                               "title",
                               "description",
                               "source",
                               "original_store",
                               "version",
                               "cursor",
                               "format")
external_object.optional <- c("primary_not_supplement",
                              "description",
                              "original_store")

quality_controlled.writable <- c("object")
quality_controlled.readable <- c("url",
                                 "last_updated",
                                 "updated_by",
                                 quality_controlled.writable)
quality_controlled.queryable <- c("updated_by",
                                  "last_updated",
                                  "object",
                                  "cursor",
                                  "format")
quality_controlled.optional <- c()

keyword.writable <- c("object",
                      "keyphrase")
keyword.readable <- c("url",
                      "last_updated",
                      "updated_by",
                      keyword.writable)
keyword.queryable <- c("updated_by",
                       "last_updated",
                       "object",
                       "keyphrase",
                       "cursor",
                       "format")
keyword_optional <- c()

author.writable <- c("object",
                     "family_name",
                     "personal_name")
author.readable <- c("url",
                     "last_updated",
                     "updated_by",
                     author.writable)
author.queryable <- c("updated_by",
                      "last_updated",
                      "object",
                      "family_name",
                      "personal_name",
                      "cursor",
                      "format")
author.optional <- c()

licence.writable <- c("object",
                      "licence_info")
licence.readable <- c("url",
                      "last_updated",
                      "updated_by",
                      licence.writable)
licence.queryable <- c("updated_by",
                       "last_updated",
                       "object",
                       "licence_info",
                       "cursor",
                       "format")
licence_optional <- c()

namespace.writable <- c("name")
namespace.readable <- c("url",
                        "last_updated",
                        "updated_by",
                        namespace.writable)
namespace.queryable <- c("data_products",
                         "updated_by",
                         "last_updated",
                         "name",
                         "cursor",
                         "format")
namespace.optional <- c()

data_product.writable <- c("name",
                           "version",
                           "object",
                           "namespace")
data_product.readable <- c("url",
                           "last_updated",
                           "updated_by",
                           data_product.writable)
data_product.queryable <- c("data_products",
                            "updated_by",
                            "last_updated",
                            "name",
                            "cursor",
                            "format")
data_product.optional <- c()

code_repo_release.writable <- c("name",
                                "version",
                                "object",
                                "website")
code_repo_release.readable <- c("url",
                                "last_updated",
                                "updated_by",
                                code_repo_release.writable)
code_repo_release.optional <- c("website")

key_value.writable <- c("object",
                        "key",
                        "value")
key_value.readable <- c("url",
                        "last_updated",
                        "updated_by",
                        key_value.writable)
key_value.queryable <- c("updated_by",
                         "last_updated",
                         "text",
                         "cursor",
                         "format")
key_value.optional <- c()

text_file.writable <- c("text")
text_file.readable <- c("url",
                        "last_updated",
                        "updated_by",
                        text_file.writable)
text_file.queryable <- c("updated_by",
                         "last_updated",
                         "text",
                         "cursor",
                         "format")
text_file.optional <- c()

#' check table exists
#'
#' @param table name of table
#'
#' @return boolean if a table exists
#'
#' @export
#'
check_table_exists <- function(table){
  if(! table %in% table_list)
    return(FALSE)
  return(TRUE)
}

#' Get Writable Fields
#'
#' @param table name of table
#'
#' @return a character vector of writable fields
#'
#' @export
#'
get_table_writable <- function(table){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get(paste0(table, ".writable")))
}

#' Get Readable Field
#'
#' @param table name of table
#'
#' @return a character vector of readable fields
#'
#' @export
#'
get_table_readable <- function(table){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get(paste0(table, ".readable")))
}

#' Get Queryable Fields
#'
#' @param table name of table
#'
#' @return a character vector of queryable fields
#'
#' @export
#'
get_table_queryable <- function(table){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get(paste0(table, ".queryable")))
}

#' Get Optional Fields
#'
#' @param table name of table
#'
#' @return a character vector of optional fields
#'
#' @export
#'
get_table_optional <- function(table){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get(paste0(table, ".optional")))
}

#' Is table writable
#'
#' @param table name of table
#'
#' @return boolean if a table is writable
#'
#' @export
#'
is_table_writable <- function(table){
  if(is.null(get_table_writable(table)))
     return(FALSE)
  return(TRUE)
}

#' Get Required Fields
#'
#' @param table name of table
#'
#' @return a character vector of required fields
#'
#' @export
#'
get_table_required <- function(table){
  if(! check_table_exists(table))
    stop("Unknown Table")
  if(!is_table_writable(table))
    stop("Table not writable")
  if(is.null(get_table_optional(table)))
    return(get_table_writable(table))
  return(get_table_writable(table)[!is.element(get_table_writable(table), get_table_optional(table))])
}

#' is queryable
#' Produces error \code{Unknown Table} if table does not exist
#'
#' @param table name of table
#' @param query_parameter a string or vector of strings to check
#'
#' @return either true / false if a single string is provided or a vector or true or false if vector is provided
#'
#' @export
#'
is_queryable <- function(table, query_parameter)
{
  if(is.null(get_table_queryable(table)))
    return(FALSE)
  return(query_parameter %in% get_table_queryable(table))
}

#' check query
#' produces error if table does not exist or if query is not a list
#'
#' @param table name of table
#' @param query query to check
#'
#' @return boolean if the query is valid for the table
#'
#' @export
#'
check_query <-function(table, query){
  if(!is.list(query))
    stop("Invalid query type")
  return(all(is_queryable(table, names(query))))
}
