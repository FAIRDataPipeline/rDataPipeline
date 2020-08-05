#' upload_paper
#'
#' @param title a \code{string} specifying the title of the paper
#' @param authors a \code{string} specifying the authors
#' @param journal a \code{string} specifying the full journal name
#' @param journal_abbreviation a \code{string} specifying the journal
#' abbreviation
#' @param journal_website a \code{string} specifying the journal homepage
#' @param release_date a \code{POSIXct} of format "%Y-%m-%d %H:%M:%S"
#' specifying the release date of the paper
#' @param abstract a \code{string} specifying the abstract
#' @param keywords a \code{string} specifying keywords or keyphrases
#' seperated by "and", e.g. "keyword1 and keyword2 and key phrase1 and keyword3"
#' @param doi a \code{string} specifying the doi
#' @param primary_not_supplement (optional) an object of class \code{logical}
#' where \code{TRUE} is the primary source (default) and false is a supplement
#' @param version (optional) a \code{string} specifying the version number
#' @param key key
#'
#' @export
#'
#' @examples
#' title <- "Covid-19: A systemic disease treated with a wide-ranging approach: A case report"
#' authors <- "Massabeti, Rosanna and Cipriani, Maria Stella and Valenti, Ivana"
#' journal <- "Journal of Population Therapeutics and Clinical Pharmacology"
#' journal_abbreviation <- "J Popul Ther Clin Pharmacol"
#' journal_website <- "https://www.jptcp.com/index.php/jptcp"
#' release_date <- as.POSIXct("2020-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")
#' abstract <- "At the end of December 2019, the Health Commission of the"
#' keywords <- "covid-19 and coronavirus disease and monoclonal antibodies
#' + and non-invasive mechanical ventilation and treatment"
#' doi <- "10.15586/jptcp.v27iSP1.691"
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

  # Check if paper exists in the data registry ----------------------------

  check_exists("external_object",
               list(doi_or_unique_name = paste0("doi://", doi)))


  # Check if journal exists in the data registry ----------------------------

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

  for(i in seq_along(authorList)) {
    tmp <- strsplit(authorList[[i]], ", ")[[1]]
    new_author(family_name = tmp[1],
               personal_name = tmp[2],
               object_id = objectId,
               key = key)
  }

  # Keywords
  if(!is.na(keywords)) {
    if(grepl("and", keywords)) {
      keywordList <- as.list(strsplit(keywords, " and ")[[1]])
    } else {
      keywordList <- list(keywords)
    }

    for(i in seq_along(keywordList)) {
      new_keyword(keyphrase = keywordList[[i]],
                  object_id = objectId,
                  key = key)
    }
  }


  # Paper metadata
  new_external_object(doi_or_unique_name = paste0("doi://", doi),
                      primary_not_supplement = primary_not_supplement,
                      release_date = release_date,
                      title = title,
                      description = abstract,
                      version = version,
                      object_id = objectId,
                      source_id = sourceId, # journal
                      original_store_id = "", # pdf website
                      key = key)
}
