\name{Soil_HBV}

\alias{Soil_HBV}

\title{
Conceptual soil moisture routine to account for abstractions and effective runoff.
}

\description{
Calculates effective runoff to be routed in the buckets (\code{\link{Routing_HBV}}).
}

\usage{
Soil_HBV(model, inputData, initCond, param)
}


\arguments{
  \item{model}{
choose model's option as numeric. \enumerate{
\item Classical HBV soil moisture routine.
\item HBV soil moisture routine with varying area. This option should be used with SnowGlacier_HBV's \emph{model 3}.
}%end enumerate
}% end model

  \item{inputData}{
numeric matrix with columns as input data.

\strong{Model 1} \enumerate{
\item \code{Total}: \code{Mice + Msnow + Prain} \eqn{[mm/\Delta t]}.
\item \code{PET}: potential evapotranspiration serie \eqn{[mm/\Delta t]}.
} %end Model 1

\strong{Model 2} \enumerate{
\item \code{Total}: \code{Mice + Msnow + Prain} \eqn{[mm/\Delta t]}.
\item \code{PET}: potential evapotranspiration serie \eqn{[mm/\Delta t]}.
\item \code{SoCA} : serie of relative soil area (ratio of soil surface over basin area).
} %end Model 2

}%end inputData

  \item{initCond}{
numeric vector with the following values: \enumerate{
\item \code{SM}: initial soil water content \eqn{[mm]}.
\item \code{realtArea}: relative area \eqn{[-]}.
}%end enumerate
}%end initCond

  \item{param}{
numeric parameter's vector: \enumerate{
\item \code{FC}: fictitious soil field capacity \eqn{[mm]}.
\item \code{LP}: parameter to get actual ET \eqn{[-]}.
\item \eqn{\beta}: parameter that allows non-linear relations between input-output runoff \eqn{[-]}.
}%end enumerate
}%end param

}%end arguments

\value{
Returns a numeric matrix with the following columns: \itemize{
\item \code{Rech}: recharge serie \eqn{[mm/\Delta t]}. This is the input to \code{\link{Routing_HBV}}
module.
\item \code{Eact}: actual evaporation/evapotranspiration serie \eqn{[mm/\Delta t]}.
\item \code{SM}: soil moisture serie \eqn{[mm/\Delta t]}.
}%end itemize
}%end value

\references{
Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545. https://doi.org/10.1002/hyp.10510
}

\author{
J. Ezequiel Toum A.
}

\examples{
# HBV soil routine with variable area
  ## Calder's model
potEvap <- PET(model = 1, hemis = 1, inputData = as.matrix(1:315), elev = c(1000, 1500),
param = c(4, 0.5))

  ## Debris-covered ice
ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
ObsPrecip <- runif(n = 315, max = 50, min = 0)
ObsGCA    <- seq(1, 0.8, -0.2/314)

  ## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
DebrisCovGlac <- SnowGlacier_HBV(model = 3, inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
                initCond = c(10, 3, 1), param = c(1, 1, 0, 3, 1, 6))

  ## Soil routine
  ObsSoCA     <- 1 - ObsGCA
  inputMatrix <- cbind(DebrisCovGlac[ , 9], potEvap, ObsSoCA)

soil <- Soil_HBV(model = 2, inputData = inputMatrix, initCond = c(50), param = c(200, 0.5, 2))
}
