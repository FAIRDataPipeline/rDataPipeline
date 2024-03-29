% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_read_match.R
\name{find_read_match}
\alias{find_read_match}
\title{Find matching read aliases in config file}
\usage{
find_read_match(handle, data_product)
}
\arguments{
\item{handle}{an object of class \code{fdp, R6} containing metadata required
by the Data Pipeline API}

\item{data_product}{a \code{string} specifying the data product name}
}
\description{
Find read aliases in working config that match wildcard string
}
