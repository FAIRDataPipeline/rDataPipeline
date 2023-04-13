#' Validate fields
#'
#' Function to validate fields in post data
#' 
#' @keywords internal
#'
#' @param table table name as character
#' @param data data as a named list
#' @param endpoint endpoint
#'
#' @return Returns
#'
validate_fields <- function(table, data, endpoint) {

  table.fields <- get_fields(table, endpoint = endpoint)

  table_required <- table.fields %>%
    filter(.data$required)

  # Check object
  if (is.null(data) & nrow(table_required) == 0) {
    warning("Data is Null")
  } else if (!is.list(data)) {
    stop("data must be a list")
  }

  # Ensure fields are writable
  table.writable <- table.fields %>%
    filter(!.data$read_only)
  fields_required <- character(0)
  data_field_names <- names(data)

  for (field_i in seq_along(table_required$field)) {
    type <- table_required[field_i, ]$data_type
    name <- table_required[field_i, ]$field
    # If a field is plural it can sometimes be not required or empty even
    # though they're listed as required, e.g component_issues and object_issues
    # on issues or input_of and output_of on object component
    doesnt_end_in_of <- grepl(".*?s_of$", name)
    doesnt_end_in_s <- grepl(".*?s$", name)
    if (type == "field" & (doesnt_end_in_s | doesnt_end_in_of)) {
      #
    } else
      fields_required <- c(fields_required, name)
  }

  if (!all(data_field_names %in% table.writable$field))
    stop(paste0("Invalid field names valid names are: ",
                paste0(table.writable$field, collapse = " ")))

  # Ensure all required fields are present
  if (!all(fields_required %in% data_field_names))
    stop(paste0("required fields are missing required field names are: ",
                paste0(table_required, collapse = " ")))

  # Loop through data list
  for (i in seq_along(data)) {

    # Extract relevent variables from data and writable fields
    value <- data[[i]]
    name <- names(data[i])
    type <- table.writable[table.writable$field == name, ]$data_type
    max_length <- table.writable[table.writable$field == name, ]$max_length
    min_length <- table.writable[table.writable$field == name, ]$min_length
    choice_values <- table.writable[table.writable$field == name, ]$choice_values
    choice_names <- table.writable[table.writable$field == name, ]$choice_names

    # Allow empty string (cast to character as a posix date will cause an error)
    # or null if an optional field
    name_missing <- !name %in% table_required$field
    char_value <- isTRUE(is.character(value) & as.character(value) == "")
    null_value <- isTRUE(is.null(value) | length(value) == 0)

    # If name is missing ------------------------------------------------------
    if (name_missing & (char_value | null_value)) {
      # Do nothing. Empty string in an optional field will be set to NULL in
      # the database

      # If the type is a field, expect api urls  ---------------------------
    } else if (type == "field") {
      # Check if the field is plural
      if (grepl(".*?s$", name) |  grepl(".*?s_of$", name)) {
        # if so expect a list
        if (is.list(value) & length(value) == 0) {
          # allow empty list
        } else if (!is.list(value)) {
          stop(paste0(name, " must be a list"))
        } else {
          # if a list expect a list of api objects
          for (i in seq_along(value)) {
            if (!grepl(paste0(endpoint, ".+/.+"), value[[i]])) {
              stop(paste0(name, " Must be an api url"))
            } else if (!get_entity(value[[i]])$url == value[[i]]) {
              stop(paste0(name, " must be in the data registry"))
            }
          }
        }

        # If the field isn't plural expect a character of an api url
      } else if (is.character(value)) {
        if (!grepl(paste0(endpoint, ".+/.+"), value)) {
          stop(paste0(name, " Must be an api url"))
        } else if (!get_entity(value)$url == value) {
          stop(paste0(name, " must be in the data registry"))
        }
      } else {
        stop(paste0(name, " field must be either a character or a list"))
      }

      # If field is a string ------------------------------------------------

    } else if (type == "string") {
      # API will accept anything castable to a string so check it can be cast
      # to a string (character)
      if (suppressWarnings(is.na(as.character(value))))
        stop(paste0(name, " must be of type ", type))

      # If there is a maximum length, check the character does not exceed it
      if (!is.na(max_length)) {
        if (!is.na(max_length) & nchar(value) > max_length)
          stop(paste0("Maximum Length of string exceeded Max Length: ",
                      max_length))
      }

      if (name == "version") {
        if (!grepl("^.*[.].*[.].*$", value))
          stop(paste0(name, " must be of syntatic type version e.g. 0.1.0"))
      }

      # If the field is a date ----------------------------------------------

    } else if (type == "datetime") {
      # Check the date can be cast to a posix
      if (is.na.POSIXlt(value)) {
        stop(paste0(name, " must be of type ", type))
      }

      # If the field is a URL ----------------------------------------------

    } else if (type == "url") {
      # Expect a character
      if (!is.character(value)) {
        stop(paste0(name, " must be of type ", type))
      }
      # Expect there to be a :// to allow for https:// http:// ftp:// etc.
      else if (!grepl("://", value)) {
        stop(paste0("Expecting a URL got: ", value))
      }

      # If the field is a choice -------------------------------------------

    } else if (type == "choice") {
      # Expect either a string or an integer
      if (!is.character(value) & !is.integer(value) & !is.numeric(value)) {
        stop(paste0(name, " must be of type ", type))
      }
      # Expect a valid choice
      else{
        if (!grepl(as.character(value), choice_values)) {
          stop(paste0("Must be a valid choice, valid choices are: ",
                      choice_values, " Which correspond to ", choice_names))
        }
      }

      # If the field is a integer -------------------------------------------

    } else if (type == "integer") {
      # Accept a string or integer
      if (!is.character(value) & !is.integer(value) & !is.numeric(value)) {
        stop(paste0(name, " must be of type ", type))
      } else{
        # If a character make sure it's a valid integer
        if (suppressWarnings(is.na(as.integer(value))))
          stop(paste0(name, " must be of type ", type))

        if (!is.na(min_length) & as.integer(value) < min_length)
          stop(paste0(name, " must be greater than ", min_length))

        if (!is.na(max_length) & as.integer(value) > max_length)
          stop(paste0(name, " must be less than ", max_length))
      }

      # If the field is a boolean -------------------------------------------

    } else if (type == "boolean") {
      # Accept character
      if (!is.character(value) & !is.logical(value)) {
        stop(paste0(name, " must be of type ", type))
      } else if (is.character(value)) {
        if (!tolower(value) == "true" & !tolower(value) == "false") {
          stop(paste0(name, " must be of type ", type))
        }
      }
    }
  }
  data
}
