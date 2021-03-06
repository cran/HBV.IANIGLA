% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semi_distributed_hbv.R
\docType{data}
\name{semi_distributed_hbv}
\alias{semi_distributed_hbv}
\title{Semi-distributed HBV model data}
\format{
A list with five elements
 \describe{
    \item{basin}{data frame containing elevation band names and the hypsometric values
    for modeling the catchment.}
    \item{tair}{numeric matrix with the air temperature series (columns) for the 15
    elevation bands.}
    \item{prec}{numeric matrix with the precipitation series (columns) for the 15
    elevation bands.}
    \item{pet}{numeric matrix with the potential evapotranspiration series (columns) for
    the 15 elevation bands.}
    \item{qout}{numeric vector with the synthetic catchment discharge.}
 }
}
\usage{
semi_distributed_hbv
}
\description{
Here you will find the \strong{lumped model's} next step. A semi-distributed model seems more
similar to what we try to simulate in real world hydrology. This dataset
allows you to experiment with a synthetic HBV.IANIGLA semi-distributed exercise.
}
\keyword{datasets}
