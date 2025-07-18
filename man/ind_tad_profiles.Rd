% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{ind_tad_profiles}
\alias{ind_tad_profiles}
\title{ind_tad_profiles}
\usage{
ind_tad_profiles(data, meta_data = NULL, n_rows = 2, n_cols = 3)
}
\arguments{
\item{data}{A data.frame of data}

\item{meta_data}{A data.frame of meta data}

\item{n_rows}{Number of rows in the plot grid, default is 2}

\item{n_cols}{Number of columns in the plot grid, default is 3}
}
\description{
Plot DV, PRED and IPRED vs TAD
}
\examples{

# Simulate 1-compartment model
pk_data <- data_501 |>
dplyr::mutate(CL = 2, V = 40)
ind_tad_profiles(pk_data, meta_data_501)

}
