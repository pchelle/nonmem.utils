% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-figures.R
\name{base_vpc_plot}
\alias{base_vpc_plot}
\title{base_vpc_plot}
\usage{
base_vpc_plot(data, vpc_data, variable_labels, ci = 0.8, type = "vpc")
}
\arguments{
\item{data}{A data.frame of data}

\item{vpc_data}{A data.frame of VPC data}

\item{variable_labels}{A list of variable labels}

\item{ci}{Confidence interval for the VPC plot}

\item{type}{Type of VPC plot, one of \code{"vpc"}, \code{"pc_vpc"}, \code{"pvc_vpc"}, \code{"blq"}, \code{"npde"}}
}
\value{
A ggplot object
}
\description{
Base vpc plot
}
\keyword{internal}
