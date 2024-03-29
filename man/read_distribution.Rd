% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_distribution.R
\name{read_distribution}
\alias{read_distribution}
\title{Read distribution component from TOML file}
\usage{
read_distribution(handle, data_product, component)
}
\arguments{
\item{handle}{an object of class \code{fdp, R6} containing metadata required
by the Data Pipeline API}

\item{data_product}{a \code{string} specifying a data product}

\item{component}{a \code{string} specifying a data product component}
}
\description{
Function to read distribution type data from toml file.
}
