#' fdp-class
#'
#' Contained for class \code{fdp}.
#'
#' @name fdp-class
#' @rdname fdp-class
#'
fdp <- R6::R6Class("fdp", list(
  #' @field yaml working config.yaml contents
  #' @field model_config object URI associated with config.yaml storage location
  #' @field submission_script object URI associated with submission script storage
  #' @field code_run object URI associated with code run
  #' @field inputs metadata associated with code run inputs
  #' @field outputs metadata associated with code run outputs
  #'
  yaml = NULL,
  model_config = NULL,
  submission_script = NULL,
  code_run = NULL,
  inputs = NULL,
  outputs = NULL,
  issues = NULL,

  #' @description
  #' Create a new fdp object
  #' @param yaml working config.yaml contents
  #' @param model_config object URI associated with config.yaml storage location
  #' @param submission_script object URI associated with submission script storage location
  #' @param code_run object URI associated with code run
  #' @return A new `fdp` object.
  #'
  initialize = function(yaml,
                        model_config,
                        submission_script,
                        code_run) {

    stopifnot(is.list(yaml))
    stopifnot(is.character(model_config), length(model_config) == 1)
    stopifnot(is.character(submission_script), length(submission_script) == 1)
    stopifnot(is.character(code_run), length(code_run) == 1)

    self$yaml <- yaml
    self$model_config <- model_config
    self$submission_script <- submission_script
    self$code_run <- code_run

    invisible(self)
  },

  #' @description
  #' Print function
  #'
  #' @export
  #'
  print = function(...) {

    contains_yaml <- !is.null(self$yaml)
    contains_config <- !is.null(self$model_config)
    contains_script <- !is.null(self$submission_script)
    contains_coderun <- !is.null(self$code_run)

    cat("Contains:\n")
    if (contains_yaml) cat("- Config file\n")
    if (contains_config) cat("- Config file URI\n")
    if (contains_script) cat("- Submission script URI\n")
    if (contains_coderun) cat("- Code run URI\n")

    if (!is.null(self$inputs)) {
      cat("\n\n", "Inputs:", "\n")
      self$inputs %>% dplyr::select(name) %>%
        print()
    }

    if (!is.null(self$outputs)) {
      cat("\n\n", "outputs:", "\n")
      lapply(seq_len(nrow(self$outputs)), function(x) {
        tmp <- self$outputs[x,] %>%
          dplyr::select(index, component, data_product, version)
        cat(tmp$component, "\n")
      })
    }

    invisible(self)
  },

  #' @description
  #' Add inputs field
  #' @param name text
  #' @param type text
  #' @param doi_or_unique_name external object field
  #' @param title external object field
  #' @param version text
  #' @param path text
  #' @param object_id text
  #'
  input = function(name,
                   type,
                   doi_or_unique_name,
                   title,
                   version,
                   path,
                   object_id) {

    index <- get_index(self)

    if (is.null(self$inputs)) {
      existing <- data.frame(index = numeric(),
                             name = character(),
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
                      name = name,
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
                             component_uri = character(),
                             dataproduct_uri = character())
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
                      component_uri = NA,
                      dataproduct_uri = NA)
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
  #' @param index text
  #' @param component text
  #' @param data_product text
  #' @param external_object text
  #' @param version text
  #' @param namespace text
  #' @param issue text
  #' @param severity text
  #'
  raise_issue = function(index,
                         component,
                         data_product,
                         external_object,
                         version,
                         namespace,
                         issue,
                         severity) {

    if (is.null(self$issues)) {
      existing <- data.frame(index = numeric(),
                             component = character(),
                             data_product = character(),
                             external_object = character(),
                             version = character(),
                             namespace = character(),
                             issue = character(),
                             severity = numeric())
    } else {
      existing <- self$issues
    }

    new <- data.frame(index = index,
                      component = component,
                      data_product = data_product,
                      external_object = external_object,
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
  #' @param component_url text
  #'
  write_component_url = function(data_product,
                                 component,
                                 component_url) {

    index <- which(self$outputs$data_product == data_product &
                     self$outputs$component == component)

    self$outputs$component_url[index] <- component_url

    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product text
  #' @param data_product_url text
  #' @param version text
  #' @param hash text
  #'
  write_dataproduct_url = function(data_product,
                                   data_product_url,
                                   version,
                                   hash) {

    index <- which(self$outputs$data_product == data_product)

    if (length(index) != 0) {
      self$outputs$dataproduct_uri[index] <- data_product_url
      self$outputs$version[index] <- version
      self$outputs$hash[index] <- hash
    }

    invisible(self)
  }
))
