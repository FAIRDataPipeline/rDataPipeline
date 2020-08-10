% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_definitions.R
\name{get_fields}
\alias{get_fields}
\title{get fields from table
use api endpoint to produce a list of fields for a table
requires api key}
\usage{
get_fields(table, key, filter_fields = "all")
}
\arguments{
\item{table}{name of table}

\item{key}{api key / token}

\item{filter_fields}{which fields you want returned
\emph{e.g.} all, none, readable, writable, optional}
}
\value{
a character vector of fields or a list of fields if filter fields is set to "none"
}
\description{
get fields from table
use api endpoint to produce a list of fields for a table
requires api key
}