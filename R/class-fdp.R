#' fdp-class
#'
#' Contained for class \code{fdp}.
#'
#' @name fdp-class
#' @rdname fdp-class
#'
fdp <- R6::R6Class("fdp", list(
  #' @field yaml text
  #' @field model_config text
  #' @field submission_script text
  #' @field inputs text
  #' @field outputs text
  #'
  yaml = NULL,
  model_config = NULL,
  submission_script = NULL,
  inputs = NULL,
  outputs = NULL,
  issues = NULL,

  #' @description
  #' Create a new fdp object
  #' @param yaml text
  #' @param model_config text
  #' @param submission_script text
  #' @return A new `fdp` object.
  #'
  initialize = function(yaml,
                        model_config,
                        submission_script) {

    stopifnot(is.list(yaml))
    stopifnot(is.character(model_config), length(model_config) == 1)
    stopifnot(is.character(submission_script), length(submission_script) == 1)

    self$yaml <- yaml
    self$model_config <- model_config
    self$submission_script <- submission_script

    invisible(self)
  },

  #' @description
  #' Print function
  #'
  #' @export
  #'
  print = function(...) {
    cat(" yaml:", !is.null(self$yaml))
    cat("\n", "model_config:", !is.null(self$model_config))
    cat("\n", "submission_script:", !is.null(self$submission_script))

    if (!is.null(self$inputs)) {
      cat("\n\n", "inputs:", "\n")
      self$inputs %>% dplyr::select(-path, -alias) %>%
        print()
    }

    if (!is.null(self$outputs)) {
      cat("\n\n", "outputs:", "\n")
      self$outputs %>% dplyr::select(-component_id, -dataproduct_id) %>%
        print()
    }

    invisible(self)
  },

  #' @description
  #' Add inputs field
  #' @param alias text
  #' @param type text
  #' @param doi_or_unique_name text
  #' @param title text
  #' @param version text
  #' @param path text
  #' @param object_id text
  #'
  input = function(alias,
                   type,
                   doi_or_unique_name,
                   title,
                   version,
                   path,
                   object_id) {

    index <- get_index(self)

    if (is.null(self$inputs)) {
      existing <- data.frame(index = numeric(),
                             alias = character(),
                             type = character(),
                             doi_or_unique_name = character(),
                             title = character(),
                             version = character(),
                             path = character(),
                             object_id = character())
    } else {
      existing <- self$inputs
    }

    new <- data.frame(index = index,
                      alias = alias,
                      type = type,
                      doi_or_unique_name = doi_or_unique_name,
                      title = title,
                      version = version,
                      path = path,
                      object_id = object_id)
    self$inputs <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product text
  #' @param path text
  #' @param component text
  #' @param description text
  #' @param version text
  #'
  write_dataproduct = function(data_product,
                               path,
                               component,
                               description,
                               version) {

    index <- get_index(self)

    if (is.null(self$outputs)) {
      existing <- data.frame(index = numeric(),
                             data_product = character(),
                             path = character(),
                             component = character(),
                             description = character(),
                             version = character(),
                             hash = character(),
                             component_id = character(),
                             dataproduct_id = character())
    } else {
      existing <- self$outputs
    }

    new <- data.frame(index = index,
                      data_product = data_product,
                      path = path,
                      component = component,
                      description = description,
                      version = version,
                      hash = NA,
                      component_id = NA,
                      dataproduct_id = NA)
    self$outputs <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Return index
  #' @param this_data_product text
  #' @param this_component text
  #' @param this_version text
  #'
  output_index = function(this_data_product,
                          this_component,
                          this_version) {

    self$outputs %>% dplyr::filter(.data$data_product == this_data_product,
                                   .data$component == this_component,
                                   .data$version == this_version) %>%
      select(index) %>% unlist() %>% unname()
  },

  #' @description
  #' Add issues field
  #' @param component_id text
  #' @param component text
  #' @param data_product text
  #' @param version text
  #' @param namespace text
  #' @param issue text
  #' @param severity text
  #'
  raise_issue_component = function(component_id,
                                   component,
                                   data_product,
                                   version,
                                   namespace,
                                   issue,
                                   severity) {

    if (is.null(self$issues)) {
      existing <- data.frame(id = numeric(),
                             component = character(),
                             data_product = character(),
                             version = character(),
                             namespace = character(),
                             issue = character(),
                             severity = numeric())
    } else {
      existing <- self$issues
    }

    new <- data.frame(id = component_id,
                      component = component,
                      data_product = data_product,
                      version = version,
                      namespace = namespace,
                      issue = issue,
                      severity = severity)
    self$issues <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Add issues field
  #' @param data_product_id text
  #' @param data_product text
  #' @param version text
  #' @param namespace text
  #' @param issue text
  #' @param severity text
  #'
  raise_issue_dataproduct = function(data_product_id,
                                     data_product,
                                     version,
                                     namespace,
                                     issue,
                                     severity) {

    if (is.null(self$issues)) {
      existing <- data.frame(id = numeric(),
                             component = character(),
                             data_product = character(),
                             version = character(),
                             namespace = character(),
                             issue = character(),
                             severity = numeric())
    } else {
      existing <- self$issues
    }

    new <- data.frame(id = data_product_id,
                      component = NA,
                      data_product = data_product,
                      version = version,
                      namespace = namespace,
                      issue = issue,
                      severity = severity)
    self$issues <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product text
  #' @param component text
  #' @param component_id text
  #'
  write_component_id = function(data_product,
                                component,
                                component_id) {

    index <- which(self$outputs$data_product == data_product &
                     self$outputs$component == component)

    self$outputs$component_id[index] <- component_id

    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product text
  #' @param data_product_id text
  #' @param version text
  #' @param hash text
  #'
  write_dataproduct_id = function(data_product,
                                  data_product_id,
                                  version,
                                  hash) {

    index <- which(self$outputs$data_product == data_product)

    if (length(index) != 0) {
      self$outputs$dataproduct_id[index] <- data_product_id
      self$outputs$version[index] <- version
      self$outputs$hash[index] <- hash
    }

    invisible(self)
  }
))
