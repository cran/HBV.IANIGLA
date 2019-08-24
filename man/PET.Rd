\name{PET}
\alias{PET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates potential evapotranspiration
}
\description{
Choose one of the model's options to obtain a series of potential evapotranspiration
}
\usage{
PET(model, hemis, inputData, elev, param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
choose model's option as numeric: \enumerate{
  \item Calder's model.
  }
}
  \item{hemis}{
choose hemisphere option as numeric.
\enumerate{
  \item southern hemisphere.
  \item northern hemisphere.
  }
This option is harmless when using other model than \emph{Calder's}, nevertheless a numeric value must be provided.
}
  \item{inputData}{
  \describe{
    \strong{Calder's model}: numeric matrix with julian dates. e.g: \code{as.matrix(c(1:365))}.
  }
}
  \item{elev}{ \describe{
    \strong{Calder's model}: numeric vector of length two \code{c(zref, ztopo)}. \code{zref}: the reference heigh where
    potential evapotranspiration or input data to calculate PET is known. \code{ztopo}: target
    PET's topographic height.}
}
  \item{param}{ \describe{
    \strong{Calder's model}: numeric vector of length two \code{c(PET, gradPET)}. \code{PET}: climatological daily mean potential
    evapotranspiration [mm]. \code{gradPET}: evapotranspiration decrease gradient [mm/100 m].
  }
 }
}

\value{
Returns a numeric vector class with the potential evapotranspiration (in \eqn{[mm / \Delta t]}) for every time step.
}
\references{
Calder, I.R., Harding, R.J., Rosier, P.T.W., 1983. An objective assessment of soil-moisture deficit models. J. Hydrol. 60, 329–355. https://doi.org/10.1016/0022-1694(83)90030-6
}
\author{
J. Ezequiel Toum A.
}
\examples{
## Run the model for a year in the southern hemisphere
potEvap <- PET(model = 1, hemis = 1, inputData = as.matrix(1:365),
elev = c(1000, 1500), param = c(4, 0.5))
}
