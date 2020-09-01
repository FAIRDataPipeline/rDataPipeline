#' upload_paper
#'
#' @param title a \code{string} specifying the title of the paper *e.g.*
#' "Covid-19: A systemic disease treated with a wide-ranging approach: A case
#' report"
#' @param authors a \code{string} specifying the authors *e.g.* "Massabeti,
#'  Rosanna and Cipriani, Maria Stella and Valenti, Ivana"
#' @param journal a \code{string} specifying the full journal name *e.g.*
#' "Journal of Population Therapeutics and Clinical Pharmacology"
#' @param journal_abbreviation a \code{string} specifying the journal
#' abbreviation *e.g.* "J Popul Ther Clin Pharmacol"
#' @param journal_website a \code{string} specifying the journal homepage *e.g.*
#' "https://www.jptcp.com/index.php/jptcp"
#' @param release_date a \code{POSIXct} of format "%Y-%m-%d %H:%M:%S"
#' specifying the release date of the paper *e.g.*
#' as.POSIXct("2020-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")
#' @param abstract a \code{string} specifying the abstract *e.g.* "At the end
#' of December 2019, the Health Commission of the"
#' @param keywords a \code{string} specifying keywords or keyphrases
#' seperated by "and", *e.g.* "covid-19 and coronavirus disease and monoclonal
#' antibodies
#' + and non-invasive mechanical ventilation and treatment"
#' @param doi a \code{string} specifying the doi *e.g.*
#' "10.15586/jptcp.v27iSP1.691"
#' @param primary_not_supplement (optional) an object of class \code{logical}
#' where \code{TRUE} is the primary source (default) and false is a supplement
#' @param version (optional) a \code{string} specifying the version number;
#' by default this is 1.0.0 (assuming a published paper), set to 0.1.0 for a
#' preprint
#' @param key API token from data.scrc.uk
#'
#' @export
#'
upload_paper <- function(title,
                         authors,
                         journal,
                         journal_abbreviation,
                         journal_website,
                         release_date,
                         abstract,
                         keywords,
                         doi,
                         primary_not_supplement = TRUE,
                         version = "1.0.0",
                         key) {

  # If paper exists in the data registry return it's URI, otherwise create it
  if(check_exists("external_object",
                  list(doi_or_unique_name = paste0("doi://", doi)))) {
    message("external_object ", paste0("doi://", doi),
            " (doi_or_unique_name) already exists in the data registry")

    return(get_url("external_object",
                   list(doi_or_unique_name = paste0("doi://", doi))))

  } else {

    # If journal exists in the data registry return sourceId, otherwise
    # add entry
    if(check_exists("source", list(name = journal))) {
      sourceId <- get_url("source", list(name = journal))

    } else {
      sourceId <- new_source(name = journal,
                             abbreviation = journal_abbreviation,
                             website = journal_website,
                             key = key)
    }


    # Add paper metadata ------------------------------------------------------

    objectId <- new_object(storage_location_id = "",
                           key = key)

    # Authors
    if(grepl("and", authors)) {
      authorList <- as.list(strsplit(authors, " and ")[[1]])
    } else {
      authorList <- list(authors)
    }

    issue <- FALSE
    for(i in seq_along(authorList)) {
      # If an author has both a family and personal name create entry in the
      # data registry, otherwise generate an issue
      if(grepl(", [A-Za-z]+", authorList[[i]])) {
        tmp <- strsplit(authorList[[i]], ", ")[[1]]
        new_author(family_name = tmp[1],
                   personal_name = tmp[2],
                   object_id = objectId,
                   key = key)
      } else
        issue <- TRUE
    }

    # Keywords
    if(!is.na(keywords)) {
      if(grepl("and", keywords)) {
        keywordList <- as.list(strsplit(keywords, " and ")[[1]])
      } else {
        keywordList <- list(keywords)
      }

      for(i in seq_along(keywordList)) {
        tmp <- keywordList[[i]]
        # Remove square brackets (the data registry doesn't like them)
        if(grepl("\\[", keywordList[[i]]))
          tmp <- gsub("\\[", "", tmp)
        if(grepl("\\]", keywordList[[i]]))
          tmp <- gsub("\\]", "", tmp)

        new_keyword(keyphrase = tmp,
                    object_id = objectId,
                    key = key)
      }
    }

    # Paper metadata
    externalObjectId <- new_external_object(
      doi_or_unique_name = paste0("doi://", doi),
      primary_not_supplement = primary_not_supplement,
      release_date = release_date,
      title = title,
      description = abstract,
      version = version,
      object_id = objectId,
      source_id = sourceId, # journal
      original_store_id = "", # pdf website
      key = key)

    if(issue) {
      msg <- "One or more authors have not been attached to this paper"
      attach_issue(description = msg,
                   severity = 5,
                   external_object_doi = paste0("doi://", doi),
                   key = key)
    }

    return(externalObjectId)

  }

}
