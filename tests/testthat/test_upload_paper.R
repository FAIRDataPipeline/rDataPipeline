context("Testing upload_paper")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")

# user_id <- "3"

# skip_test <- FALSE
#
# release_date <- NULL
# abstract <- NULL
# object <- NULL
# first_author <- NULL
# authors <- NULL
# source <- NULL
# journal_name <- NULL
# journal_abbreviation <- NULL
# website <- NULL
# keywords <- NULL
# doi <- NULL
#
# title <- "COVID-19-like symptoms observed in Chinese tree shrews infected with SARS-CoV-2"
#
# existing_paper <- get_entry("external_object", list(title = title))[[1]]
#
# if(is.null(existing_paper)){
#   skip_test <- NULL
# } else{
#   doi <- existing_paper$doi_or_unique_name
#
#   release_date <- existing_paper$release_date
#   abstract <- existing_paper$description
#
#   version <- existing_paper$version
#
#   object <- get_entity("object", basename(existing_paper$object))
#   authors_list <- object$authors
#   for(author in seq_along(authors_list)){
#     first_author <- get_entity("author", basename(object$authors[[author]]))
#     current_author_name <- paste(first_author$family_name, first_author$personal_name, sep = ", ")
#     if(!is.null(authors))
#       authors <- paste(authors, current_author_name, sep = " and ")
#     else
#       authors <- current_author_name
#   }
#
#   source <- get_entity("source", basename(existing_paper$source))
#   journal_name <- source$name
#   journal_abbreviation <- source$abbreviation
#   website <- source$website
#
#   if(length(object$keywords) > 0){
#     for(keyword in seq_along(object$keywords)){
#       current_keyword <- get_entity("keyword", basename(object$keywords[[keyword]]))$keyphrase
#       if(is.null(keywords))
#         keywords <- current_keyword
#       else
#         keywords <- paste(keywords, current_keyword, sep = " and ")
#     }
#
#   }
#
#
#
# }
#
#
# test_that("Existing Paper returns existing paper", {
#   skip_if(skip_test)
#   expect_message(expect_true(is.character(upload_paper(title, authors, journal_name, journal_abr, website, release_date, abstract, keywords, doi, version = version, key = token))))
# })

test_identifier <- sample(1:1000000, 1, replace=TRUE)

keywords <- paste(sample(letters, 12, FALSE), collapse ="", sep = "")

date_time <- Sys.time()
formatted_date <- format(date_time, "%d%m%y%H%M%S")
doi <- paste0(formatted_date, test_identifier, "/TEST")
release_date <- date_time
title <- paste0("TEST ", formatted_date, test_identifier)
abstract <- paste0("Test Abstract ", formatted_date, test_identifier)
journal_name <- paste0("TEST Journal ", date_time, test_identifier)
journal_abr <- paste0("TEST_", formatted_date, test_identifier)
website <- ""
personal_name <- paste(sample(letters, 8, FALSE), collapse ="", sep = "")
family_name <- paste(sample(letters, 8, FALSE), collapse ="", sep = "")
authors <- paste(family_name, personal_name, sep = ", ")

test_that("Upload Paper returns ID", {
  paper_id <- character(0)
  expect_silent(paper_id <- upload_paper(title,
                                         authors,
                                         journal_name,
                                         journal_abr,
                                         website,
                                         release_date,
                                         abstract,
                                         keywords,
                                         doi,
                                         key = token))
  expect_true(is.character(paper_id))
})

authors <- paste0("TEST", formatted_date)

test_that("incorrect authors returns a message", {
  expect_message(paper_id <- upload_paper(title,
                                         authors,
                                         journal_name,
                                         journal_abr,
                                         website,
                                         release_date,
                                         abstract,
                                         keywords,
                                         doi,
                                         key = token))
})
