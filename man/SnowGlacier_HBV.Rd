\name{SnowGlacier_HBV}

\alias{SnowGlacier_HBV}


\title{
Snowmelt and ice-melt models
}

\description{
Simulates the accumulation and melt processes of snow and ice using a temperature index model.
}

\usage{
SnowGlacier_HBV(model, inputData, initCond, param)
}


\arguments{
  \item{model}{
choose model's option as numeric. \enumerate{
\item Temperature index model.
\item Temperature index model with snow cover area.
\item Temperature index model with variable glacier area.
}%end enumerate
}%end model
  \item{inputData}{
numeric matrix with columns as input data.

\strong{Model 1} \enumerate{
\item \code{airT}: air temperature \eqn{[°C/\Delta t]}.
\item \code{precip}: precipitation \eqn{[mm/\Delta t]}.
} %end Model 1

\strong{Model 2} \enumerate{
\item \code{airT}: air temperature \eqn{[°C/\Delta t]}.
\item \code{precip}: precipitation \eqn{[mm/\Delta t]}.
\item \code{SCA}: snow cover area. Values between [0 ; 1] \eqn{[-]}.
} %end Model 2

\strong{Model 3} \enumerate{
\item \code{airT}: air temperature  \eqn{[°C/\Delta t]}.
\item \code{precip}: precipitation  \eqn{[mm/\Delta t]}.
\item \code{GCA}: glacier cover area. This area values are relative to the
total surface area of the basin \eqn{[-]}.
} %end Model 3
}% end inputData
  \item{initCond}{
numeric vector with the following values. \itemize{
\item \code{SWE0}: initial snow water equivalent \eqn{[mm]}.
\item numeric integer indicating the surface type. \emph{1}: clean ice; \emph{2}: soil;
\emph{3}: debris-covered ice.
\item area of the glacier(s) (in the elevation band) relative to the basin; e.g.: 0.1 \eqn{[-]}.
This option is required in \emph{Model 1} and \emph{Model 2} when surface is a glacier.
}%end itemize
}
  \item{param}{
numeric vector with the following values: \enumerate{
\item \code{SFCF}: snowfall correction factor \eqn{[-]}.
\item \code{Tr}: solid and liquid precipitation threshold temperature \eqn{[ºC]}.
\item \code{Tt}: melt temperature \eqn{[ºC]}.
\item \code{fm}: snowmelt factor \eqn{[mm/°C.\Delta t]}.
\item \code{fi}: ice-melt factor \eqn{[mm/°C.\Delta t]}.
\item \code{fic}: debris-covered ice-melt factor \eqn{[mm/°C.\Delta t]}.
}%end enumerate
}%end param
}%end arguments

\value{
Numeric matrix with the following columns:

\strong{Model 1}

** if surface is soil, \enumerate{
\item \code{Prain}: precip. as rainfall.
\item \code{Psnow}: precip. as snowfall.
\item \code{SWE}: snow water equivalent.
\item \code{Msnow}: melted snow.
\item \code{Total}: \code{Prain} + \code{Msnow}.
}%end enumerate

** if surface is ice, \enumerate{
\item \code{Prain}: precip. as rainfall.
\item \code{Psnow}: precip. as snowfall.
\item \code{SWE}: snow water equivalent.
\item \code{Msnow}: melted snow.
\item \code{Mice}: melted ice.
\item \code{Mtot}: \code{Msnow} + \code{Mice}.
\item \code{Cum}: \code{Psnow} - \code{Mtot}.
\item \code{Total}: \code{Prain} + \code{Mtot}.
\item \code{TotScal}: \code{Total} * initCond[3].
}%end enumerate

\strong{Model 2}

** if surface is soil,\enumerate{
\item \code{Prain}: precip. as rainfall.
\item \code{Psnow}: precip. as snowfall.
\item \code{SWE}: snow water equivalent.
\item \code{Msnow}: melted snow.
\item \code{Total}: \code{Prain} + \code{Msnow}.
\item \code{TotScal}: \code{Msnow} * \code{SCA} + \code{Prain}.
}%end enumerate

** if surface is ice -> as in \emph{Model 1}

\strong{Model 3}

** if surface is soil -> as in \emph{Model 1}

** if surface is ice -> as in \emph{Model 1}

}%end value

\references{
Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545. https://doi.org/10.1002/hyp.10510

Parajka, J., Merz, R., Blöschl, G., 2007. Uncertainty and multiple objective calibration in regional water balance modelling: case study in 320 Austrian catchments. Hydrol. Process. 21, 435–446. https://doi.org/10.1002/hyp.6253

Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325. https://doi.org/10.5194/hess-16-3315-2012
}

\author{
J. Ezequiel Toum A.
}

\examples{
## Debris-covered ice
ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
ObsPrecip <- runif(n = 315, max = 50, min = 0)
ObsGCA    <- seq(1, 0.8, -0.2/314)

## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
DebrisCovGlac <- SnowGlacier_HBV(model = 3, inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
                initCond = c(10, 3, 1), param = c(1, 1, 0, 3, 1, 6))

}
