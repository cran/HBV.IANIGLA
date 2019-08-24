\name{Precip_model}

\alias{Precip_model}

\title{Gradient precipitation models}

\description{
Contains a linear precipitation gradient model to extrapolate raingauge measurements from a fixed elevation to
other sites with different elevation. Also included is a modified version which incorporates an extra parameter to avoid increasing precipitation beyond a certain altitude.
}

\usage{
Precip_model(model, inputData, zmeteo, ztopo, param)
}

\arguments{
  \item{model}{
choose model's option as numeric:
\enumerate{
\item Linear precip. gradient (LP).
\item Linear precip. gradient with upper threshold (LPM).

 }%end enumerate
}%end model
  \item{inputData}{
  \strong{LP}: numeric vector with precipitation serie \eqn{[mm/\Delta t]}.

  \strong{LPM}: numeric vector with precipitation serie \eqn{[mm/\Delta t]}.
}
  \item{zmeteo}{
  altitude of the raingauge \eqn{[masl]}.

}
  \item{ztopo}{
  target heigh \eqn{[masl]}.

}
  \item{param}{
  \strong{LP}: numeric vector with precipitation gradient \code{gradP} [\%/100m].

  \strong{LPM}: numeric vector with two values. \code{gradP}: precip. gradient [\%/100m].

  \code{maxALT}: threshold heigh where precip. does not increase any more \eqn{[masl]}.

}
}

\value{
Numeric vector with the extrapolated precipitation.
}
\references{
For some interesting work on precipitation gradients in basin and synoptic scale see:

Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014. The importance of observed gradients of air temperature and precipitation for modeling runoff from a glacierized watershed in the Nepalese Himalayas. Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506

Viale, M., Nuñez, M.N., 2010. Climatology of Winter Orographic Precipitation over the Subtropical Central Andes and Associated Synoptic and Regional Characteristics. J. Hydrometeorol. 12, 481–507. https://doi.org/10.1175/2010JHM1284.1
}

\author{
J. Ezequiel Toum A.
}

\examples{
## LP case
set.seed(369)

precLP <- Precip_model(model = 1, inputData = runif(n = 365, max = 30, min = 0),
          zmeteo = 3000, ztopo = 4700, param = c(5))

## LPM case
set.seed(369)

precLPM <- Precip_model(model = 2, inputData = runif(n = 365, max = 30, min = 0),
          zmeteo = 3000, ztopo = 4700, param = c(5, 4500))

}
