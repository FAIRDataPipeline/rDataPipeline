context("Testing attach_issue")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

datetime <- format(Sys.time(), "%d%m%y%H%M%S")

UID <- paste0("external object ", datetime, test_identifier)
UID_name <- paste0("external object ", datetime, test_identifier)
doi <- paste0("externalobject", datetime, test_identifier)
abbreviation <- paste0("EO ", datetime, test_identifier)
path <- paste0(UID_name, ".h5")
path_uri <- paste0("https://", path)
hash <- sha1(UID)

object_id <- post_data("object",
                       list(description = UID),
                       key)


source_id <- get_entry("source", list(updated_by = test_user))[[1]]$url
original_store_id <- get_entry("storage_location", list(updated_by = test_user))[[1]]$url
storage_root_id <- get_entry("storage_root", list(updated_by = test_user))[[1]]$url

if(is.null(source_id)){
  source_id <- post_data("source",
                         list(name = UID,
                              abbreviation = abbreviation),
                         key)
}

if(is.null(storage_root_id)){
  storage_root_id <- post_data("storage_root",
                               list(name = UID,
                                    root = path_uri),
                               key)
}

if(is.null(original_store_id)){
  original_store_id <- post_data("storage_location",
                                 list(path = path,
                                      hash = hash,
                                      storage_root = storage_root_id),
                                 key)
}

external_object <- new_external_object(doi,
                       TRUE,
                       Sys.time(),
                       UID,
                       UID,
                       create_version_number(),
                       object_id,
                       source_id,
                       original_store_id,
                       key)
external_object_id <- basename(external_object)
external_object_doi <- get_entity("external_object", external_object_id)$doi_or_unique_name

severity <- "5"

test_that("attach isssue works with external_object_doi", {
  skip_if(is.null(external_object_id))
  expect_message(attach_issue(UID,
                 severity,
                 list(external_object_doi = external_object_doi,
                 version = "0.1.0"),
                 key = key
                 ))
  issue <- get_entry("issue", list(description = UID))
  expect_true(object_id %in% issue[[1]]$object_issues)
})

test_identifier <- sample(1:1000000, 1, replace=TRUE)
UID <- paste0("data_product ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

object_id <- post_data("object",
                       list(description= UID),
                       key)


namespace_id <- post_data("namespace",
                          list(name = UID),
                          key)

new_data_product_id <- new_data_product(UID,
                                            create_version_number(),
                                            object_id,
                                            namespace_id,
                                            key)


test_that("attach isssue works with data product", {
  skip_if(is.null(external_object_id))
  expect_message(attach_issue(UID,
                              severity,
                              list(data_product = UID,
                                   namespace = UID,
                                   version = create_version_number()),
                              key = key
  ))
  issue <- get_entry("issue", list(description = UID))
  expect_true(object_id %in% issue[[1]]$object_issues)
})

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID_component <- paste0("object component", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

object_component_id <- new_object_component(UID_component,
                                            object_id,
                                            UID,
                                            key)

test_that("attach isssue works with object component", {
  skip_if(is.null(external_object_id))
  expect_message(attach_issue(UID,
                              severity,
                              list(data_product = UID,
                                   namespace = UID,
                                   component = UID_component,
                                   version = create_version_number()),
                              key = key
  ))
  issue <- get_entry("issue", list(description = UID))
  expect_true(object_component_id %in% issue[[1]]$component_issues)
})

test_that("attach issue errors as it should",{
  expect_error(attach_issue(UID,
               severity,
               list(external_object_doi = external_object_doi),
               key))
  expect_error(attach_issue(UID,
               severity,
               list(data_product = UID),
               key))
  expect_error(attach_issue(UID,
                            severity,
                            list(data_product = UID,
                                 component = UID),
                            key))
})
