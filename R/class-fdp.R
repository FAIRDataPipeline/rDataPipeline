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
  #' @param data_product data_product
  #' @param use_data_product use_data_product
  #' @param use_component use_component
  #' @param use_version use_version
  #' @param use_namespace namespace
  #' @param path path
  #' @param component_url component_url
  #'
  input = function(data_product,
                   use_data_product,
                   use_component,
                   use_version,
                   use_namespace,
                   path,
                   component_url) {

    index <- get_index(self)

    if (is.null(self$inputs)) {
      existing <- data.frame(index = numeric(),
                             data_product = character(),
                             use_data_product = character(),
                             use_component = character(),
                             use_version = character(),
                             use_namespace = character(),
                             path = character(),
                             component_url = character())
    } else {
      existing <- self$inputs
    }

    new <- data.frame(index = index,
                      data_product = data_product,
                      use_data_product = use_data_product,
                      use_component = use_component,
                      use_version = use_version,
                      use_namespace = use_namespace,
                      path = path,
                      component_url = component_url)
    self$inputs <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Add outputs field
  #' @param data_product data_product
  #' @param use_data_product use_data_product
  #' @param use_component use_component
  #' @param use_version use_version
  #' @param use_namespace use_namespace
  #' @param path path
  #' @param description description
  #'
  output = function(data_product,
                    use_data_product,
                    use_component,
                    use_version,
                    use_namespace,
                    path,
                    description) {

    index <- get_index(self)

    if (is.null(self$outputs)) {
      existing <- data.frame(index = numeric(),
                             data_product = character(),
                             use_data_product = character(),
                             use_component = character(),
                             use_version = character(),
                             use_namespace = character(),
                             path = character(),
                             description = character(),
                             hash = character(),
                             data_product_url = character(),
                             component_url = character(),
                             registered_data_product = logical())
    } else {
      existing <- self$outputs
    }

    new <- data.frame(index = index,
                      data_product = data_product,
                      use_data_product = use_data_product,
                      use_component = use_component,
                      use_version = use_version,
                      use_namespace = use_namespace,
                      path = path,
                      description = description,
                      hash = NA,
                      data_product_url = NA,
                      component_url = NA,
                      registered_data_product = FALSE)
    self$outputs <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Return index
  #' @param data_product use_data_product
  #' @param component use_component
  #' @param version use_version
  #'
  output_index = function(data_product,
                          component,
                          version) {

    self$outputs %>% dplyr::filter(.data$use_data_product == data_product,
                                   .data$use_component == component,
                                   .data$use_version == version) %>%
      select(index) %>% unlist() %>% unname()
  },

  #' @description
  #' Add issues field
  #' @param index text
  #' @param use_data_product text
  #' @param use_component text
  #' @param use_version text
  #' @param use_namespace text
  #' @param issue text
  #' @param severity text
  #'
  raise_issue = function(index,
                         use_data_product,
                         use_component,
                         use_version,
                         use_namespace,
                         issue,
                         severity) {

    if (is.null(self$issues)) {
      existing <- data.frame(index = numeric(),
                             use_data_product = character(),
                             use_component = character(),
                             use_version = character(),
                             use_namespace = character(),
                             issue = character(),
                             severity = numeric())
    } else {
      existing <- self$issues
    }

    new <- data.frame(index = index,
                      use_data_product = use_data_product,
                      use_component = use_component,
                      use_version = use_version,
                      use_namespace = use_namespace,
                      issue = issue,
                      severity = severity)
    self$issues <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description
  #' Add file hash to outputs and re-write path name
  #' @param use_data_product text
  #' @param use_version text
  #' @param use_namespace text
  #' @param hash text
  #' @param new_path text
  #'
  finalise_output_hash = function(use_data_product,
                                  use_version,
                                  use_namespace,
                                  hash,
                                  new_path) {

    index <- which(self$outputs$use_data_product == use_data_product &&
                     self$outputs$use_namespace == use_namespace &&
                     self$outputs$use_version == use_version)

    if (length(index) == 0) {
      stop("Handle not updated")

    } else {
      self$outputs$hash[index] <- hash
      self$outputs$path[index] <- new_path
    }

    invisible(self)
  },
  #' @description
  #' Add outputs field
  #' @param use_data_product text
  #' @param use_component text
  #' @param data_product_url text
  #' @param component_url text
  #'
  finalise_output_url = function(use_data_product,
                                 use_component,
                                 data_product_url,
                                 component_url) {

    # Update handle with component URL
    if (is.na(use_component)) {
      index <- which(self$outputs$use_data_product == use_data_product &
                       is.na(self$outputs$use_component))
    } else {
      index <- which(self$outputs$use_data_product == use_data_product &
                       self$outputs$use_component == use_component)
    }

    self$outputs$component_url[index] <- component_url

    # If a data product URL is already in the handle, check it matches
    if (!is.na(self$outputs$data_product_url) &&
        self$outputs$data_product_url != data_product_url)
      stop("Something went wrong")

    # Update handle with data product URL
    index_data_products <- which(self$outputs$use_data_product %in%
                                   use_data_product)
    self$outputs$data_product_url[index_data_products] <- data_product_url

    # Update handle with flag to show data product has already been registered
    self$outputs$registered_data_product[index_data_products] <- TRUE

    invisible(self)
  }
))
