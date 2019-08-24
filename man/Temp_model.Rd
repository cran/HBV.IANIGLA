\name{Temp_model}

\alias{Temp_model}

\title{
Linear temperature gradient model
}

\description{
Contains a linear temperature gradient model to extrapolate air temperature measurements from fixed stations to other sites.
}

\usage{
Temp_model(model, inputData, zmeteo, ztopo, param)
}

\arguments{
  \item{model}{
choose model's option as numeric: \enumerate{
\item linear gradient model.
}%end enumerate
}%end model

  \item{inputData}{
numeric vector with the air temperature serie \eqn{[ºC/\Delta t]}.
}%end inputData

  \item{zmeteo}{
altitude of the air temperature sensor \eqn{[masl]}.
}%end zmeteo

  \item{ztopo}{
target heigh \eqn{[masl]}.
}%end ztopo

  \item{param}{
numeric vector with the following values,

\strong{Model 1} \enumerate{
\item \code{gradT}: air temperature gradient factor \eqn{[°C/100m]}.
} %end enumerate model 1
}%end param
}%end arguments

\value{
Returns a numeric vector with the extrapolated air temperature serie.
}%end values

\references{
Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014. The importance of observed gradients of air temperature and precipitation for modeling runoff from a glacierized watershed in the Nepalese Himalayas. Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506
}

\author{
J. Ezequiel Toum A.
}

\examples{
airTemp <- Temp_model(model = 1, inputData = runif(200, max = 25, min = -10),
           zmeteo = 2000, ztopo = 3500, param = c(-0.65))
}
