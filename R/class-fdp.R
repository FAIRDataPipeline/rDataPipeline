#' fdp-class
#'
#' Container for class \code{fdp}
#'
#' @name fdp-class
#' @rdname fdp-class
#'
fdp <- R6::R6Class("fdp", public = list(
  #' @field yaml a \code{list} containing the contents of the working
  #' config.yaml
  #' @field fdp_config_dir a \code{string} specifying the directory passed
  #' from `fair run`
  #' @field model_config a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the \code{storage_location} of the
  #' working config.yaml
  #' @field submission_script a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the \code{storage_location} of the
  #' submission script
  #' @field code_repo a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the GitHub repository
  #' @field code_run a \code{string} specifying the URL of an entry in
  #' the \code{code_run} table
  #' @field inputs a \code{data.frame} containing metadata associated with
  #' \code{code_run} inputs
  #' @field outputs a \code{data.frame} containing metadata associated with
  #' \code{code_run} outputs
  #' @field issues a \code{data.frame} containing metadata associated with
  #' \code{code_run} issues
  #'
  yaml = NULL,
  fdp_config_dir = NULL,
  model_config = NULL,
  submission_script = NULL,
  code_repo = NULL,
  code_run = NULL,
  inputs = NULL,
  outputs = NULL,
  issues = NULL,

  #' @description Create a new \code{fdp} object
  #'
  #' @param yaml a \code{list} containing the contents of the working
  #' config.yaml
  #' @param fdp_config_dir  a \code{string} specifying the directory passed
  #' from `fair run`
  #' @param model_config a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the \code{storage_location} of the
  #' working config.yaml
  #' @param submission_script a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the \code{storage_location} of the
  #' submission script
  #' @param code_repo a \code{string} specifying the URL of an entry in
  #' the \code{object} table associated with the GitHub repository
  #' @param code_run a \code{string} specifying the URL of an entry in
  #' the \code{code_run} table
  #'
  #' @return Returns a new \code{fdp} object
  #'
  initialize = function(yaml,
                        fdp_config_dir,
                        model_config,
                        submission_script,
                        code_repo,
                        code_run) {

    stopifnot(is.list(yaml))
    stopifnot(is.character(fdp_config_dir), length(fdp_config_dir) == 1)
    stopifnot(is.character(model_config), length(model_config) == 1)
    stopifnot(is.character(submission_script), length(submission_script) == 1)
    stopifnot(is.character(code_run), length(code_run) == 1)

    self$yaml <- yaml
    self$fdp_config_dir <- fdp_config_dir
    self$model_config <- model_config
    self$submission_script <- submission_script
    self$code_repo <- code_repo
    self$code_run <- code_run

    invisible(self)
  },

  #' @description Print method
  #' @param ... additional parameters, currently none are used
  #' @export
  #'
  print = function(...) {

    contains_yaml <- !is.null(self$yaml)
    contains_dir <- !is.null(self$fdp_config_dir)
    contains_config <- !is.null(self$model_config)
    contains_script <- !is.null(self$submission_script)
    contains_coderun <- !is.null(self$code_run)

    cat("Contains:\n")
    if (contains_yaml) cat("- Config file\n")
    if (contains_dir) cat("- Config dir\n")
    if (contains_config) cat("- Config file URL\n")
    if (contains_script) cat("- Submission script URL\n")
    if (contains_coderun) cat("- Code run URL\n")

    if (!is.null(self$inputs)) {
      cat("\n\n", "Inputs:", "\n")
      self$inputs %>%
        dplyr::select(data_product) %>%
        print()
    }

    if (!is.null(self$outputs)) {
      cat("\n\n", "outputs:", "\n")
      tmp <- self$outputs %>%
        dplyr::select(use_component, use_data_product, use_version)
      names(tmp) <- gsub("use_", "", names(tmp))
      print(tmp)
    }

    invisible(self)
  },

  #' @description Record \code{code_run} inputs in \code{fdp} object
  #'
  #' @param data_product a \code{string} specifying the name of the data
  #' product, used as a reference
  #' @param use_data_product a \code{string} specifying the name of the data
  #' product, used as input in the \code{code_run}
  #' @param use_component a \code{string} specifying the name of the data
  #' product component, used as input in the \code{code_run}
  #' @param use_version a \code{string} specifying the data product version,
  #' used as input in the \code{code_run}
  #' @param use_namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as input in the \code{code_run}
  #' @param path a \code{string} specifying the location of the data product
  #' in the local data store
  #' @param component_url a \code{string} specifying the URL of an entry in the
  #' \code{object_component} table
  #'
  #' @return Returns an updated \code{fdp} object
  #'
  input = function(data_product,
                   use_data_product,
                   use_component,
                   use_version,
                   use_namespace,
                   path,
                   component_url) {

    index <- create_index(self)

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

  #' @description Record \code{code_run} outputs in \code{fdp} object
  #'
  #' @param data_product a \code{string} specifying the name of the data
  #' product, used as a reference
  #' @param use_data_product a \code{string} specifying the name of the data
  #' product, used as output in the \code{code_run}
  #' @param use_component a \code{string} specifying the name of the data
  #' product component, used as output in the \code{code_run}
  #' @param use_version a \code{string} specifying the version of the data
  #' product, used as output in the \code{code_run}
  #' @param use_namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as output in the \code{code_run}
  #' @param path a \code{string} specifying the location of the data product
  #' in the local data store
  #' @param data_product_description a \code{string} containing a description of
  #' the data product
  #' @param component_description a \code{string} containing a description of
  #' the data product component
  #' @param public
  #'
  #' @return Returns an updated \code{fdp} object
  #'
  output = function(data_product,
                    use_data_product,
                    use_component,
                    use_version,
                    use_namespace,
                    path,
                    data_product_description,
                    component_description,
                    public) {

    index <- create_index(self)

    if (is.null(self$outputs)) {
      existing <- data.frame(index = numeric(),
                             data_product = character(),
                             use_data_product = character(),
                             use_component = character(),
                             use_version = character(),
                             use_namespace = character(),
                             path = character(),
                             data_product_description = character(),
                             component_description = character(),
                             public = logical(),
                             hash = character(),
                             data_product_url = character(),
                             component_url = character(),
                             delete_if_duplicate = logical())
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
                      data_product_description = data_product_description,
                      component_description = component_description,
                      public = public,
                      hash = NA,
                      data_product_url = NA,
                      component_url = NA,
                      delete_if_duplicate = NA)
    self$outputs <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description Return index of data product recorded in \code{fdp} object
  #' so that an issue may be attached
  #'
  #' @param data_product a \code{string} specifying the name of the data
  #' product, used as output in the \code{code_run}
  #' @param component a \code{string} specifying the name of the data
  #' product component, used as output in the \code{code_run}
  #' @param version a \code{string} specifying the name of the data
  #' product version, used as output in the \code{code_run}
  #' @param namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as input in the \code{code_run}
  #'
  #' @return Returns an index used to identify the data product
  #'
  output_index = function(data_product,
                          component,
                          version,
                          namespace) {

    index <- which(self$outputs$use_data_product == data_product &
                     self$outputs$use_component == component &
                     self$outputs$use_version == version &
                     self$outputs$use_namespace == namespace)

    invisible(self$outputs$index[index])
  },

  #' @description Record \code{issue} in \code{fdp} object
  #'
  #' @param index a \code{numeric} index, used to identify each input and
  #' output in the \code{fdp} object
  #' @param type a \code{string} specifying the type of issue (one of
  #' "data", "config", "script", "repo")
  #' @param use_data_product a \code{string} specifying the name of the data
  #' product, used as output in the \code{code_run}
  #' @param use_component a \code{string} specifying the name of the data
  #' product component, used as output in the \code{code_run}
  #' @param use_version a \code{string} specifying the name of the data
  #' product version, used as output in the \code{code_run}
  #' @param use_namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as input in the \code{code_run}
  #' @param issue a \code{string} containing a free text description of the
  #' \code{issue}
  #' @param severity an \code{integer} specifying the severity of the
  #' \code{issue}
  #'
  #' @return Returns an updated \code{fdp} object
  #'
  raise_issue = function(index,
                         type,
                         use_data_product,
                         use_component,
                         use_version,
                         use_namespace,
                         issue,
                         severity) {

    if (is.null(self$issues)) {
      existing <- data.frame(index = numeric(),
                             type = character(),
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
                      type = type,
                      use_data_product = use_data_product,
                      use_component = use_component,
                      use_version = use_version,
                      use_namespace = use_namespace,
                      issue = issue,
                      severity = severity)
    self$issues <- rbind.data.frame(existing, new)

    invisible(self)
  },

  #' @description Record file hash and update path name in \code{fdp} object
  #'
  #' @param use_data_product a \code{string} specifying the name of the data
  #' product, used as output in the \code{code_run}
  #' @param use_data_product_runid a \code{string} specifying the name of the
  #' data product, the same as \code{use_data_product} excluding the RUN_ID
  #' variable
  #' @param use_version a \code{string} specifying the name of the data
  #' product version, used as output in the \code{code_run}
  #' @param use_namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as input in the \code{code_run}
  #' @param hash a \code{string} specifying the hash of the file
  #' @param new_path a \code{string} specifying the updated location (filename
  #' is now the hash of the file) of the data product in the local data store
  #' @param data_product_url a \code{string} specifying the URL of an
  #' \code{object} associated with the \code{data_product}
  #' @param delete_if_duplicate (optional) default is `FALSE`
  #'
  #' @return Returns an updated \code{fdp} object
  #'
  finalise_output_hash = function(use_data_product,
                                  use_data_product_runid,
                                  use_version,
                                  use_namespace,
                                  hash,
                                  new_path,
                                  data_product_url,
                                  delete_if_duplicate = FALSE) {

    # We want to record this data in all component rows so filtering by
    # alias is not enough
    index <- which(self$outputs$use_data_product == use_data_product &
                     self$outputs$use_namespace == use_namespace &
                     self$outputs$use_version == use_version)

    if (length(index) == 0) {
      stop("Handle not updated")

    } else {

      if (delete_if_duplicate) {
        self$outputs$use_data_product[index] <- use_data_product_runid
        self$outputs$hash[index] <- hash
        self$outputs$path[index] <- NA
        self$outputs$data_product_url[index] <- NA
        self$outputs$delete_if_duplicate[index] <- TRUE

      } else {
        self$outputs$use_data_product[index] <- use_data_product_runid
        self$outputs$hash[index] <- hash
        self$outputs$path[index] <- new_path
        self$outputs$data_product_url[index] <- data_product_url
        self$outputs$delete_if_duplicate[index] <- FALSE
      }
    }

    invisible(self)
  },

  #' @description Record \code{data_product} and component URLs in \code{fdp}
  #' object
  #'
  #' @param use_data_product a \code{string} specifying the name of the data
  #' product, used as output in the \code{code_run}
  #' @param use_component a \code{string} specifying the name of the data
  #' product component, used as output in the \code{code_run}
  #' @param use_version a \code{string} specifying the name of the data
  #' product version, used as output in the \code{code_run}
  #' @param use_namespace a \code{string} specifying the namespace in which
  #' the data product resides, used as input in the \code{code_run}
  #' @param component_url a \code{string} specifying the URL of an entry in the
  #' \code{object_component} table
  #'
  #' @return Returns an updated \code{fdp} object
  #'
  finalise_output_url = function(use_data_product,
                                 use_component,
                                 use_version,
                                 use_namespace,
                                 component_url) {

    # Update handle with component URL
    if (is.na(use_component)) {
      index <- which(self$outputs$use_data_product == use_data_product &
                       self$outputs$use_namespace == use_namespace &
                       self$outputs$use_version == use_version &
                       is.na(self$outputs$use_component))
    } else {
      index <- which(self$outputs$use_data_product == use_data_product &
                       self$outputs$use_namespace == use_namespace &
                       self$outputs$use_version == use_version &
                       self$outputs$use_component == use_component)
    }

    self$outputs$component_url[index] <- component_url

    invisible(self)
  }
))
