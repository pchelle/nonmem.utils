% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-meta-data.R
\name{pull_cat}
\alias{pull_cat}
\title{pull_cat}
\usage{
pull_cat(name, meta_data = NULL)
}
\arguments{
\item{name}{Name of variable to retrieve}

\item{meta_data}{A data.frame of meta data}
}
\description{
Parse categorical variable names from meta_data
}
\examples{
# Dictionary
meta_data_501

pull_cat("SEX", meta_data_501)

}
