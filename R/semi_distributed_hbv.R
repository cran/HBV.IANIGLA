#' Semi-distributed HBV model data
#'
#' Here you will find the \strong{lumped model's} next step. A semi-distributed model seems more
#' similar to what we try to simulate in real world hydrology. This dataset
#' allows you to experiment with a synthetic HBV.IANIGLA semi-distributed exercise.
#'
#' @format A list with five elements
#'  \describe{
#'     \item{basin}{data frame containing elevation band names and the hypsometric values
#'     for modeling the catchment.}
#'     \item{tair}{numeric matrix with the air temperature series (columns) for the 15
#'     elevation bands.}
#'     \item{prec}{numeric matrix with the precipitation series (columns) for the 15
#'     elevation bands.}
#'     \item{pet}{numeric matrix with the potential evapotranspiration series (columns) for
#'     the 15 elevation bands.}
#'     \item{qout}{numeric vector with the synthetic catchment discharge.}
#'  }
#'
"semi_distributed_hbv"
