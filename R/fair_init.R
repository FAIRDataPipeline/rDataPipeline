#' fair_init
#'
#' @param family_name a \code{string} specifying family name
#' @param given_name a \code{string} specifying given name(s)
#' @param orcid a \code{string} specifying an orcid id URL
#' @param organisation a \code{string} or vector of \code{strings} specifying
#' the name of the organisation(s)
#' @param identifier a \code{string} specifying the organisation identifer (ROR ID) URL
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @export
#'
fair_init <- function(family_name,
                      given_name,
                      orcid,
                      organisation,
                      identifier,
                      endpoint = "http://localhost:8000/api/") {

  user_url <- get_url(table = "users",
                      query = list(username = "admin"),
                      endpoint = endpoint)

  if (missing(orcid)) {
    author_url <- new_author(family_name = family_name,
                             given_name = given_name,
                             endpoint = endpoint)
  } else {
    author_url <- new_author(family_name = family_name,
                             given_name = given_name,
                             identifier = orcid,
                             endpoint = endpoint)
  }

  if (missing(ror)) {
    organisation_urls <- lapply(organisation, function(x)
      new_organisation(name = x,
                       endpoint = endpoint))
  } else {
    organisation_urls <- lapply(organisation, function(x)
      new_organisation(name = x,
                       identifier = identifier,
                       endpoint = endpoint))

  }

  new_user_author_org(user_url = user_url,
                      author_url = author_url,
                      organisations_urls = organisation_urls,
                      endpoint = endpoint)
}
