\name{Glacier_Disch}

\alias{Glacier_Disch}

\title{
Glacier discharge conceptual model
}

\description{
Conceptual storage reservoir model of glacier discharge. It follows the concepts described in \cite{Jansson et al. 2002} that were applied in the Bridge glacier by \cite{Stahl et al. 2008} (hereafter S08).
}

\usage{
Glacier_Disch(model, inputData, initCond, param)
}


\arguments{
  \item{model}{
choose model's option as numeric:\enumerate{
  \item S08 model.
  }
}
  \item{inputData}{
    \strong{S08 model}: numeric matrix with two columns:

    \code{SWE}: snow water equivalent above the glacier \eqn{[mm/\Delta t]}.

    \code{Total}: melted snow + melted ice + rainfall \eqn{[mm/\Delta t]}.


  NOTE: you can obtain the \code{SWE} and \code{Total} series from the \code{\link{SnowGlacier_HBV}} function.
}
  \item{initCond}{
  \describe{
    \strong{S08 model}: numeric value with the initial glacier reservoir value \code{SG} \eqn{[mm]}.}
}
  \item{param}{
  \describe{
    \strong{S08 model}: numeric vector with three parameters.
    \enumerate{
    \item \code{KGmin}: minimal outflow rate \eqn{[1/\Delta t]}.
    \item \code{dKG}: maximum outflow rate increase \eqn{[1/\Delta t]}.
    \item \code{AG}: scale factor \eqn{[mm]}.}
    }%% end describe
    }%% end param
} %% end arguments

\value{
Returns a numeric matrix with two columns for every time step.
\enumerate{
\item \code{Q}: glacier discharge \eqn{[mm/\Delta t]}.
\item \code{SG}: glacier's water storage content \eqn{[1/\Delta t]}.
}
}

\references{
Jansson, P., Hock, R., Schneider, T., 2003. The concept of glacier storage: a review. J. Hydrol., Mountain Hydrology and Water Resources 282, 116–129. https://doi.org/10.1016/S0022-1694(03)00258-0

Stahl, K., Moore, R.D., Shea, J.M., Hutchinson, D., Cannon, A.J., 2008. Coupled modelling of glacier and streamflow response to future climate scenarios. Water Resour. Res. 44, W02422. https://doi.org/10.1029/2007WR005956
}

\author{
J. Ezequiel Toum A.
}

\examples{
DataMatrix <- cbind(runif(n = 100, min = 0, max = 50),
                    runif(n = 100, min = 0, max = 200))

dischGl    <- Glacier_Disch(model = 1, inputData = DataMatrix,
              initCond = 100, param = c(0.1, 0.9, 10))

}
