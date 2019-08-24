\name{UH}

\alias{UH}

\title{
Unit hydrograph
}

\description{
Transfer function to adjust the simulated hydrograph timing.
}

\usage{
UH(model, Qg, param)
}


\arguments{
  \item{model}{
choose model's option as numeric: \enumerate{
\item Static triangular unit hydrograph.
}%end enumerate
}%end model

  \item{Qg}{
numeric vector with the input data. It is the output of the \code{\link{Routing_HBV}} module.
}%end Qg

  \item{param}{
numeric vector with the following values,
\strong{Model 1} \enumerate{
\item \code{Bmax}: base of the UH triangle \eqn{[timestep]}.
} %end enumerate model 1
}%end param
}%end arguments

\value{
Returns a numeric vector with the hydrograph.
}

\references{
Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545. https://doi.org/10.1002/hyp.10510
}

\author{
J. Ezequiel Toum A.
}

\examples{
## For a real world example see Manual.pdf file.
  ## Routing example
inputMatrix <- cbind(runif(n = 200, max = 100, min = 0), runif(n = 200, max = 50, min = 5),
               runif(n = 100, max = 3, min = 1))

routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
               initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))

  ## UH
  dischOut <- UH(model = 1, Qg = routeMod1[ , 1], param = 2.2)
}
