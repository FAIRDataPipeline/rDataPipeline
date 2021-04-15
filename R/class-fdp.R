#' fdp-class
#'
#' Contained for class \code{fdp}.
#'
#' @name fdp-class
#' @rdname fdp-class
#' @exportClass fdp
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

  #' @description
  #' Create a new fdp object.
  #' @param yaml text
  #' @param model_config text
  #' @param submission_script text
  #' @return A new `fdp` object.
  #'
  initialize = function(yaml, model_config, submission_script) {
    stopifnot(is.list(yaml))
    stopifnot(is.character(model_config), length(model_config) == 1)
    stopifnot(is.character(submission_script), length(submission_script) == 1)

    self$yaml <- yaml
    self$model_config <- model_config
    self$submission_script <- submission_script
    invisible(self)
  },

  # print = function(...) {
  #   cat("Person: \n")
  #   cat("  Name: ", self$model_config, "\n", sep = "")
  #   cat("  Age:  ", self$submission_script, "\n", sep = "")
  #   invisible(self)
  # },

  #' @description
  #' Add inputs field.
  #' @param alias text
  #' @param externalobject_id text
  #'
  input = function(alias, externalobject_id) {
    self$inputs[[alias]] <- externalobject_id
    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product text
  #' @param path text
  #' @param component text
  #'
  write_dataproduct = function(data_product, path, component) {
    if (data_product %in% names(self$outputs) &&
        self$outputs[[data_product]]$path != path)
      stop("Conflicting entries")

    existing <- self$outputs$dataproducts[[data_product]]$components
    updated <- c(existing, component)
    self$outputs$dataproducts[[data_product]] <- list(path = path,
                                                      components = updated)
    invisible(self)
  }
))
