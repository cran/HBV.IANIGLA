\name{Routing_HBV}

\alias{Routing_HBV}


\title{
Routing bucket type models
}

\description{
Contains the classical HBV routing scheme and adds four more bucket models to route water.
}

\usage{
Routing_HBV(model, lake, inputData, initCond, param)
}

\arguments{
  \item{model}{
  choose model's option as numeric. \enumerate{
  \item Three series of reservoirs. Lake option is allowed.
  \item Two series of reservoirs. Lake option is allowed.
  \item Two reservoirs and three outlets. Lake option is allowed.
  \item One reservoir and two outlets. Lake is NOT allowed.
  \item One reservoir and three outlets. Lake is NOT allowed.

  }%end enumerate

}
  \item{lake}{
logical argument. \code{TRUE} will enable lake option.
}
  \item{inputData}{
numeric matrix with three columns (two of them depends if you are using a lake). \itemize{
\item \code{Ieff}: effective rainfall/melted snow serie \eqn{[mm/\Delta T]}. NOTE: output of
\code{\link{Soil_HBV}} module.
\item \code{precip}: precipitation serie falling in the lake.Note that this option should
be provided when working with lakes. Your precip. serie should be appropriately scaled \eqn{[mm/\Delta T]}.
\item  \code{evap}: lake evaporation serie. As in the precipitation teh values should be
appropriately scaled \eqn{[mm/\Delta T]}.
} %end itemize

}
  \item{initCond}{
numeric vector with the following initial state variables. \itemize{
\item \code{SLZ0}: initial water content of the lower reservoir \eqn{[mm]}.
\item \code{SUZ0}: initial water content of the intermediate reservoir. It could be optional regarding model's
option \eqn{[mm]}.
\item \code{STZ0}: initial water content of the upper reservoir. It could be optional regarding model's
option \eqn{[mm]}.
}% end itemize
}
  \item{param}{
numeric vector which sizes depends upon model's choice.

\strong{Model 1} \enumerate{
\item \code{K0}: \code{STZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K1}: \code{SUZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K2}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{UZL}: maximum rate flux between \code{STZ} and \code{SUZ} \eqn{[mm/\Delta t]}.
\item \code{PERC}: maximum rate flux between \code{STZ} and \code{SUZ} \eqn{[mm/\Delta t]}.
} %end enumerate model 1

\strong{Model 2} \enumerate{
\item \code{K1}: \code{SUZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K2}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{PERC}: maximum rate flux between \code{STZ} and \code{SUZ} \eqn{[mm/\Delta t]}.
} %end enumerate model 2

\strong{Model 3} \enumerate{
\item \code{K0}: \code{SUZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K1}: \code{SUZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K2}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{UZL}: minimum water content of \code{SUZ} to get fast runoff \code{Q0} \eqn{[mm]}.
\item \code{PERC}: maximum rate flux between \code{STZ} and \code{SUZ} \eqn{[mm/\Delta t]}.
} %end enumerate model 3

\strong{Model 4} \enumerate{
\item \code{K1}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K2}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{PERC}: minimum water content of \code{SLZ} to get {Q1} runoff \eqn{[mm]}.
} %end enumerate model 4

\strong{Model 5} \enumerate{
\item \code{K0}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K1}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{K2}: \code{SLZ} storage constant \eqn{[1/\Delta t]}.
\item \code{UZL}: minimum water content of \code{SLZ} to get fast runoff \code{Q0} \eqn{[mm]}.
\item \code{PERC}: minimum water content of \code{SLZ} to get {Q1} runoff \eqn{[mm]}.
} %end enumerate model 5
} %end param description
}% end model arguments description

\value{
in every case this function returns a matrix with outlet discharges and reservoir(s) water levels series.
}

\references{
Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological modelling—experience
from the HBV approach. Hydrol. Process. 29, 3535–3545. https://doi.org/10.1002/hyp.10510

Beven, K.J., 2012. Rainfall - Runoff Modelling, 2 edition. ed. Wiley, Chichester.

Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325. https://doi.org/10.5194/hess-16-3315-2012
}

\author{
J. Ezequiel Toum A.
}

\examples{
inputMatrix <- cbind(runif(n = 200, max = 100, min = 0), runif(n = 200, max = 50, min = 5),
               runif(n = 100, max = 3, min = 1))

routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
               initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))
}
