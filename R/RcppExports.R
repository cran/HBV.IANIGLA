# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @name PET
#'
#' @title Potential evapotranspiration models
#'
#' @description Calculate your potential evapotranspiration series. This module was
#' design to provide a simple and straight forward way to calculate
#' one of the inputs for the soil routine (to show how does it works), but for real
#' world application I strongly recommend the use of the specialized
#' \href{https://CRAN.R-project.org/package=Evapotranspiration}{\code{Evapotranspiration}}
#' package.
#'
#' @usage PET(
#'   model,
#'   hemis,
#'   inputData,
#'   elev,
#'   param
#'   )
#'
#' @param model numeric value with model option:
#' \itemize{
#'   \item 1: Calder's model.
#' }
#'
#' @param hemis numeric value indicating the hemisphere:
#' \itemize{
#'   \item 1: southern hemisphere.
#'   \item 2: northern hemisphere.
#' }
#'
#' @param inputData numeric matrix with the following columns:
#'
#'  \strong{Calder's model}
#'  \itemize{
#'   \item \code{column_1}: julian dates, e.g: \code{as.matrix( c(1:365) )}.
#'  }
#'
#' @param elev numeric vector with the following values:
#'
#'  \strong{Calder's model}
#'  \itemize{
#'   \item 1: \code{zref}: the reference height where potential evapotranspiration or
#'   input data to calculate PET is known.
#'   \item 2: \code{ztopo}: target PET's topographic height.
#'  }
#'
#' @param param numeric vector with the following values:
#'
#' \strong{Calder's model}
#'  \itemize{
#'   \item 1: \code{PET}: climatological daily mean potential evapotranspiration [mm].
#'   \item 2: \code{gradPET}: evapotranspiration decrease gradient [mm/100 m].
#'  }
#'
#' @return Numeric vector with the potential evapotranspiration series.
#'
#' @references
#' Calder, I.R., Harding, R.J., Rosier, P.T.W., 1983. An objective assessment of soil-moisture
#' deficit models. J. Hydrol. 60, 329–355. https://doi.org/10.1016/0022-1694(83)90030-6
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## Run the model for a year in the southern hemisphere
#' potEvap <- PET(model = 1,
#'                hemis = 1,
#'                inputData = as.matrix(1:365),
#'                elev = c(1000, 1500),
#'                param = c(4, 0.5))
#'
#' @export
#'
PET <- function(model, hemis, inputData, elev, param) {
    .Call(`_HBV_IANIGLA_PET`, model, hemis, inputData, elev, param)
}

#' @name Glacier_Disch
#'
#' @title Glacier discharge conceptual model
#'
#' @description Implement the conceptual water storage and release formulation for
#' glacier runoff routing. The current model version follows the approach proposed
#' by \cite{Stahl et al. (2008)} (hereafter S08) for the Bridge River basin. Note that
#' the bucket storage and release concepts for glacier runoff modeling are also
#' described in \cite{Jansson et al. (2002)}.
#'
#' @usage Glacier_Disch(
#'        model,
#'        inputData,
#'        initCond,
#'        param
#'        )
#'
#' @param model numeric integer with the model's choice. The current HBV.IANIGLA version
#' only supports the \strong{S08} approach.
#'  \itemize{
#'   \item 1:\strong{S08} glacier storage and release model.
#'   }
#'
#'   \if{html}{\figure{glacier_discharge_hbv.png}{options: width=200}}
#'
#' @param inputData numeric matrix with two columns:
#'
#'  \strong{Model 1}
#'  \itemize{
#'  \item \code{column_1}: snow water equivalent above the glacier \eqn{[mm/\Delta t]}.
#'  The series can be obtained from the \link{SnowGlacier_HBV} function output.
#'  \item \code{column_2}:  melted snow + melted ice + rainfall \eqn{[mm/\Delta t]}. This
#'  series comes from the \strong{TotScal} column in the \link{SnowGlacier_HBV}
#'  function output.
#'  }
#'
#' @param initCond numeric value with the initial glacier reservoir
#' water content \strong{\code{SG}} \eqn{[mm]}.
#'
#' @param param numeric vector with the following values:
#'
#' \strong{Model 1 (S08)}
#'  \itemize{
#'  \item \code{KGmin}: minimal outflow rate \eqn{[1/\Delta t]}.
#'  \item \code{dKG}:  maximum outflow rate increase \eqn{[1/\Delta t]}.
#'  \item \code{AG}: scale factor \eqn{[mm]}.
#'  }
#'
#' @return Numeric matrix with the following columns:
#'
#' \strong{Model 1 (S08)}
#' \itemize{
#'   \item \code{Q}: glacier discharge \eqn{[mm/\Delta t]}.
#'   \item \code{SG}: glacier's bucket water storage content series \eqn{[1/\Delta t]}.
#' }
#'
#' @references
#' Jansson, P., Hock, R., Schneider, T., 2003. The concept of glacier storage: a review.
#' J. Hydrol., Mountain Hydrology and Water Resources 282, 116–129.
#' https://doi.org/10.1016/S0022-1694(03)00258-0
#'
#' Stahl, K., Moore, R.D., Shea, J.M., Hutchinson, D., Cannon, A.J., 2008. Coupled
#' modelling of glacier and streamflow response to future climate scenarios.
#' Water Resour. Res. 44, W02422. https://doi.org/10.1029/2007WR005956
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## Create an input data and run the module
#' DataMatrix <- cbind(
#'                     runif(n = 100, min = 0, max = 50),
#'                     runif(n = 100, min = 0, max = 200)
#'                     )
#'
#' dischGl    <- Glacier_Disch(model = 1, inputData = DataMatrix,
#'                            initCond = 100, param = c(0.1, 0.9, 10))
#'
#' @export
#'
#'
Glacier_Disch <- function(model, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_Glacier_Disch`, model, inputData, initCond, param)
}

#' @name Precip_model
#'
#' @title Altitude gradient based precipitation models
#'
#' @description Extrapolate precipitation gauge measurements to another heights. In this package
#' version you can use the classical linear gradient model or a modified version which
#' sets a threshold altitude for precipitation increment (avoiding unreliable estimations).
#'
#' @usage Precip_model(
#'        model,
#'        inputData,
#'        zmeteo,
#'        ztopo,
#'        param
#' )
#'
#' @param model numeric value with model option:
#' \itemize{
#'   \item 1: linear precipitation gradient (LP).
#'   \item 2: linear precipitation gradient with an upper threshold (LPM).
#' }
#'
#' @param inputData numeric vector with precipitation gauge series \eqn{[mm/\Delta t]}.
#'
#' @param zmeteo numeric value indicating the altitude of the precipitation gauge \eqn{[masl]}.
#'
#' @param ztopo numeric value with the target height \eqn{[masl]}.
#'
#' @param param numeric vector with the following parameters:
#'
#' \strong{LP}
#' \itemize{
#'   \item 1: precipitation gradient (\code{gradP}) [\%/100m].
#' }
#'
#' \strong{LPM}
#' \itemize{
#'   \item 1: precipitation gradient (\code{gradP}) [\%/100m].
#'   \item 2: threshold height. Precipitation does not increase when the altitude (\code{ztopo})
#'   is higher than this parameter \eqn{[masl]}.
#' }
#'
#' @return Numeric vector with the extrapolated precipitation series.
#'
#' @references For some interesting work on precipitation gradients at catchment and
#' synoptic scale see:
#'
#' Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014.
#' The importance of observed gradients of air temperature and precipitation for modeling
#' runoff from a glacierized watershed in the Nepalese Himalayas.
#' Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506
#'
#' Viale, M., Nuñez, M.N., 2010. Climatology of Winter Orographic Precipitation over the
#' Subtropical Central Andes and Associated Synoptic and Regional Characteristics.
#' J. Hydrometeorol. 12, 481–507. https://doi.org/10.1175/2010JHM1284.1
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#'## LP case
#' set.seed(369)
#'
#' precLP <- Precip_model(model = 1, inputData = runif(n = 365, max = 30, min = 0),
#'                         zmeteo = 3000, ztopo = 4700, param = c(5))
#'
#'## LPM case
#' set.seed(369)
#'
#' precLPM <- Precip_model(model = 2, inputData = runif(n = 365, max = 30, min = 0),
#'                         zmeteo = 3000, ztopo = 4700, param = c(5, 4500))
#'
#' @export
#'
#'
Precip_model <- function(model, inputData, zmeteo, ztopo, param) {
    .Call(`_HBV_IANIGLA_Precip_model`, model, inputData, zmeteo, ztopo, param)
}

#' @name Routing_HBV
#'
#' @title Routing bucket type models
#'
#' @description Implement one of the five different bucket formulations for
#' runoff routing. The output of this function is the input series of the
#' transfer function (\code{\link{UH}}).
#'
#' @usage Routing_HBV(
#'        model,
#'        lake,
#'        inputData,
#'        initCond,
#'        param
#'        )
#'
#' @param model numeric integer indicating which reservoir formulation to use:
#' \itemize{
#'   \item 1: Three series of reservoirs. Lake option is allowed.
#'
#'   \if{html}{\figure{bucket_3_outlet_3.png}{options: width=200}}
#'
#'   \item 2: Two series of reservoirs. Lake option is allowed.
#'
#'   \if{html}{\figure{bucket_2_outlet_2.png}{options: width=200}}
#'
#'   \item 3: Two reservoirs and three outlets. Lake option is allowed.
#'
#'   \if{html}{\figure{bucket_2_outlet_3.png}{options: width=200}}
#'
#'   \item 4: One reservoir and two outlets. Lake is NOT allowed.
#'
#'   \if{html}{\figure{bucket_1_outlet_2.png}{options: width=200}}
#'
#'   \item 5: One reservoir and three outlets. Lake is NOT allowed.
#'
#'   \if{html}{\figure{bucket_1_outlet_3.png}{options: width=200}}
#'
#'}
#'
#' @param lake logical. A \code{TRUE} value will enable the lake option (only available
#' on \strong{models 1, 2 and 3}). When modeling a lake, HBV.IANIGLA considers
#' that this water body exist on the bottom bucket, so you will also have to provide
#' a lake evaporation and precipitation series in the \strong{\code{inputData}} matrix.
#'
#' @param inputData numeric matrix with three columns (two of them depends on
#' \strong{\code{lake}} option).
#' \itemize{
#'   \item \code{column_1}: effective runoff series \eqn{[mm/\Delta T]}. This is
#'   the output of the \code{\link{Soil_HBV}} module.
#'   \item \code{column_2}: only if  \strong{\code{lake = TRUE}}. Precipitation series
#'   falling in the lake. \strong{When using it remember that the precipitation should
#'   be rescaled according to the relative area of the lake in the basin}.
#'   \item \code{column_3}: only if  \strong{\code{lake = TRUE}}. Lake's evaporation
#'   series. \strong{When using it remember that the precipitation should
#'   be rescaled according to the relative area of the lake in the basin}.
#' }
#'
#' @param initCond numeric vector with the following initial state variables.
#' \itemize{
#'   \item \code{SLZ0}: initial water content of the lower reservoir \eqn{[mm]}. This
#'   state variable is compulsory for all \strong{model} options.
#'   \item \code{SUZ0}: initial water content of the intermediate reservoir \eqn{[mm]}.
#'   This option does not make sense for \strong{models 4 and 5}.
#'   \item \code{STZ0}: initial water content of the upper reservoir \eqn{[mm]}.
#'   This option only make sense for \strong{model 1}.
#'
#' }
#'
#' @param param numeric vector. The length depends on the \strong{model}'s choice:
#'
#' \strong{Model 1}
#' \itemize{
#'   \item \code{K0}: top bucket (\code{STZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{UZL}: maximum flux rate between \code{STZ} and
#'   \code{SUZ} \eqn{[mm/\Delta t]}.
#'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
#'   \code{SLZ} \eqn{[mm/\Delta t]}.
#'}
#'
#' \strong{Model 2}
#' \itemize{
#'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
#'   \code{SLZ} \eqn{[mm/\Delta t]}.
#'}
#'
#' \strong{Model 3}
#' \itemize{
#'   \item \code{K0}: top output (\code{Q0}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{UZL}: minimum water content of \code{SUZ} for supplying fast runoff
#'   (\code{Q0}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
#'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
#'   \code{SLZ} \eqn{[mm/\Delta t]}.
#'}
#'
#' \strong{Model 4}
#' \itemize{
#'   \item \code{K1}: intermediate output (\code{Q1}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{PERC}: minimum water content of \code{SLZ} for supplying intermediate
#'   runoff (\code{Q1}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
#' }
#'
#' \strong{Model 5}
#' \itemize{
#'   \item \code{K0}: top output (\code{Q0}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K1}: intermediate output (\code{Q1}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
#'   \item \code{UZL}: minimum water content of \code{SLZ} for supplying fast runoff
#'   (\code{Q0}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
#'   \item \code{PERC}: minimum water content of \code{SLZ} for supplying intermediate
#'   runoff (\code{Q1}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
#'}
#'
#' @return Numeric matrix with the following columns:
#'
#' \strong{Model 1}
#' \itemize{
#'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q0}: top bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{STZ}: top reservoir storage \eqn{[mm]}.
#'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
#'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
#'}
#'
#'\strong{Model 2}
#' \itemize{
#'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
#'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
#'}
#'
#' \strong{Model 3}
#' \itemize{
#'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q0}: intermediate bucket fast discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
#'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
#'}
#'
#' \strong{Model 4}
#' \itemize{
#'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q1}: lower bucket intermediate discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
#'}
#'
#' \strong{Model 5}
#' \itemize{
#'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q0}: lower bucket fast discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q1}: lower bucket intermediate discharge \eqn{[mm/\Delta t]}.
#'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
#'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
#'}
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## Case example with the first model
#' inputMatrix <- cbind(
#'                      runif(n = 200, max = 100, min = 0),
#'                      runif(n = 200, max = 50, min = 5),
#'                      runif(n = 100, max = 3, min = 1)
#'                      )
#'
#' routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
#'                      initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))
#'
#'
#' @references
#' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in
#' hydrological modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
#' https://doi.org/10.1002/hyp.10510
#'
#' Beven, K.J., 2012. Rainfall - Runoff Modelling, 2 edition. ed. Wiley, Chichester.
#'
#' Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly
#' catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325.
#' https://doi.org/10.5194/hess-16-3315-2012
#'
#'
#' @export
#'
Routing_HBV <- function(model, lake, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_Routing_HBV`, model, lake, inputData, initCond, param)
}

#' @name SnowGlacier_HBV
#'
#' @title Snow and ice-melt models
#'
#' @description Allows you to simulate snow accumulation and melting processes
#' using a temperature index approach. The function also incorporates options
#' for clean and debris covered glacier surface mass balance simulations.
#'
#' @usage SnowGlacier_HBV(
#'        model,
#'        inputData,
#'        initCond,
#'        param
#' )
#'
#' @param model numeric indicating which model you will use:
#' \itemize{
#'   \item 1: temperature index model.
#'   \item 2: temperature index model with a variable snow cover area as input data
#'   (as in the Snowmelt Runoff Model - SRM).
#'   \item 3: temperature index model with a variable glacier area as input data.
#' }
#'
#' @param inputData numeric matrix being columns the input variables. As in the whole
#' package functions, \code{NA_real_}  values are forbidden. When speaking about model
#' options we refer to the \strong{\code{model}} argument.
#'
#' \strong{Model 1:} \itemize{
#' \item \code{column_1}: air temperature series \eqn{[°C/\Delta t]}.
#' \item \code{column_2}: precipitation series \eqn{[mm/\Delta t]}.
#' }
#'
#' \strong{Model 2:} \itemize{
#' \item \code{column_1}: air temperature \eqn{[°C/\Delta t]}.
#' \item \code{column_2}: precipitation \eqn{[mm/\Delta t]}.
#' \item \code{column_3}: snow cover area. Values between [0 ; 1] \eqn{[-]}.
#' }
#'
#' \strong{Model 3:} \itemize{
#' \item \code{column_1}: air temperature  \eqn{[°C/\Delta t]}.
#' \item \code{column_2}: precipitation  \eqn{[mm/\Delta t]}.
#' \item \code{column_3}: glacier cover area. This area values are relative to the
#' total surface area of the basin \eqn{[-]}.
#' }
#'
#' @param initCond numeric vector with the following values.
#'  \itemize{
#'  \item \code{SWE0}: initial snow water equivalent \eqn{[mm]}.
#'  \item numeric integer indicating the surface type. \emph{1}: clean ice; \emph{2}: soil;
#'  \emph{3}: debris-covered ice.
#'  \item area of the glacier(s) (in the elevation band) relative to the basin; e.g.: 0.1 \eqn{[-]}.
#'  This option is required in \emph{Model 1} and \emph{Model 2} when surface is a glacier.
#'  }
#'
#' @param param numeric vector with the following values:
#'  \enumerate{
#'  \item \code{SFCF}: snowfall correction factor \eqn{[-]}.
#'  \item \code{Tr}: solid and liquid precipitation threshold temperature \eqn{[ºC]}.
#'  \item \code{Tt}: melt temperature \eqn{[ºC]}.
#'  \item \code{fm}: snowmelt factor \eqn{[mm/°C.\Delta t]}.
#'  \item \code{fi}: icemelt factor \eqn{[mm/°C.\Delta t]}.
#'  \item \code{fic}: debris-covered ice-melt factor \eqn{[mm/°C.\Delta t]}.
#'  }
#'
#' @return Numeric matrix with the following columns:
#'
#' \strong{Model 1}
#'
#' ** if surface is soil,
#' \enumerate{
#'   \item \code{Prain}: precip. as rainfall.
#'   \item \code{Psnow}: precip. as snowfall.
#'   \item \code{SWE}: snow water equivalent.
#'   \item \code{Msnow}: melted snow.
#'   \item \code{Total}: \code{Prain} + \code{Msnow}.
#' }
#'
#'  ** if surface is ice,
#'  \enumerate{
#'    \item \code{Prain}: precip. as rainfall.
#'    \item \code{Psnow}: precip. as snowfall.
#'    \item \code{SWE}: snow water equivalent.
#'    \item \code{Msnow}: melted snow.
#'    \item \code{Mice}: melted ice.
#'    \item \code{Mtot}: \code{Msnow} + \code{Mice}.
#'    \item \code{Cum}: \code{Psnow} - \code{Mtot}.
#'    \item \code{Total}: \code{Prain} + \code{Mtot}.
#'    \item \code{TotScal}: \code{Total} * initCond[3].
#'  }
#'
#' \strong{Model 2}
#'
#' ** if surface is soil,
#' \enumerate{
#'    \item \code{Prain}: precip. as rainfall.
#'    \item \code{Psnow}: precip. as snowfall.
#'    \item \code{SWE}: snow water equivalent.
#'    \item \code{Msnow}: melted snow.
#'    \item \code{Total}: \code{Prain} + \code{Msnow}.
#'    \item \code{TotScal}: \code{Msnow} * \code{SCA} + \code{Prain}.
#'  }
#'
#' ** if surface is ice -> as in \emph{Model 1}
#'
#' \strong{Model 3}
#'
#' ** if surface is soil -> as in \emph{Model 1}
#'
#' ** if surface is ice,
#'  \enumerate{
#'    \item \code{Prain}: precip. as rainfall.
#'    \item \code{Psnow}: precip. as snowfall.
#'    \item \code{SWE}: snow water equivalent.
#'    \item \code{Msnow}: melted snow.
#'    \item \code{Mice}: melted ice.
#'    \item \code{Mtot}: \code{Msnow} + \code{Mice}.
#'    \item \code{Cum}: \code{Psnow} - \code{Mtot}.
#'    \item \code{Total}: \code{Prain} + \code{Mtot}.
#'    \item \code{TotScal}: \code{Total} *  inputData[i, 3].
#'  }
#'
#' @references
#' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
#' modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
#' https://doi.org/10.1002/hyp.10510
#'
#' DeWalle, D. R., & Rango, A. (2008). Principles of Snow Hydrology.
#'
#' Parajka, J., Merz, R., Blöschl, G., 2007. Uncertainty and multiple objective calibration
#' in regional water balance modelling: case study in 320 Austrian catchments.
#' Hydrol. Process. 21, 435–446. https://doi.org/10.1002/hyp.6253
#'
#' Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly
#' catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325.
#' https://doi.org/10.5194/hess-16-3315-2012
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## Debris-covered ice
#' ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
#' ObsPrecip <- runif(n = 315, max = 50, min = 0)
#' ObsGCA    <- seq(1, 0.8, -0.2/314)
#'
#' ## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
#' DebrisCovGlac <- SnowGlacier_HBV(model = 3,
#'                                  inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
#'                                  initCond = c(10, 3, 1),
#'                                  param = c(1, 1, 0, 3, 1, 6))
#'
#' @export
#'
#'
SnowGlacier_HBV <- function(model, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_SnowGlacier_HBV`, model, inputData, initCond, param)
}

#' @name Soil_HBV
#'
#' @title Empirical soil moisture routine
#'
#' @description This module allows you to account for actual evapotranspiration,
#' abstractions, antecedent conditions and effective runoff. The formulation enables
#' non linear relationships between soil box water input (rainfall plus snowmelt) and
#' the effective runoff. This effective value is the input series to the routine function
#' (\code{\link{Routing_HBV}}).
#'
#' @usage Soil_HBV(
#'        model,
#'        inputData,
#'        initCond,
#'        param
#'        )
#'
#' @param model numeric integer suggesting one of the following options:
#' \itemize{
#'   \item 1: Classical HBV soil moisture routine.
#'   \item 2: HBV soil moisture routine with varying area. This option should
#'   be used with \code{\link{SnowGlacier_HBV}}'s \strong{\emph{model 3}}.
#' }
#'
#' @param inputData numeric matrix with the following series
#'
#' \strong{Model 1}
#' \itemize{
#'   \item \code{column_1}: \code{Total = Prain + Msnow} \eqn{[mm/\Delta t]}. This
#'   series comes from the output of the \code{\link{SnowGlacier_HBV}} module.
#'   \item \code{column_2}: potential evapotranspiration  \eqn{[mm/\Delta t]}. Since
#'   the package has a simple model (\code{\link{PET}}) to obtain this
#'   series I strongly recommend using the
#'   \href{https://CRAN.R-project.org/package=Evapotranspiration}{\code{Evapotranspiration}}
#'   package.
#' }
#'
#' \strong{Model 2}
#' \itemize{
#'   \item \code{column_1}: as in \strong{model 1}.
#'   \item \code{column_2}: as in \strong{model 1}.
#'   \item \code{column_3} : relative soil area (ratio of soil surface over
#'   basin area). When the glacier area changes the soil does the same, so coherence
#'   between this two series should be seek.This value is used to scale the effective
#'   runoff accordingly (\code{Rech} column in the matrix output).
#' }
#'
#' @param initCond numeric vector with the following values:
#'  \enumerate{
#'   \item initial soil water content \eqn{[mm]}. This is a model state variable
#'   and is internally used as first soil moisture value.
#'   \item relative area \eqn{[-]}. Only needed when using \strong{\code{model 1}}.
#'   This is the soil surface proportion relative to the catchment as a whole, so
#'   the values should never supersede one (1). This value is used to scale the effective
#'   runoff accordingly (\code{Rech} column in the matrix output).
#'}
#'
#' @param param numeric vector with the following values:
#' \enumerate{
#'   \item \code{FC}: fictitious soil field capacity \eqn{[mm]}.
#'   \item \code{LP}: parameter to get actual ET \eqn{[-]}.
#'   \item \eqn{\beta}: exponential value that allows for non-linear relations between
#'   soil box water input (rainfall plus snowmelt) and the effective runoff \eqn{[-]}.
#' }
#'
#' @return Numeric matrix with the following columns:
#' \enumerate{
#'   \item \code{Rech}: recharge series \eqn{[mm/\Delta t]}. This is the input to
#'   the \code{\link{Routing_HBV}} module.
#'   \item \code{Eact}: actual evapotranspiration series \eqn{[mm/\Delta t]}.
#'   \item \code{SM}: soil moisture series \eqn{[mm/\Delta t]}.
#' }
#'
#' @references
#' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
#' modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
#' https://doi.org/10.1002/hyp.10510
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' # HBV soil routine with variable area
#' ## Calder's model
#' potEvap <- PET(model = 1, hemis = 1, inputData = as.matrix(1:315), elev = c(1000, 1500),
#'               param = c(4, 0.5))
#'
#' ## Debris-covered ice
#'  ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
#'  ObsPrecip <- runif(n = 315, max = 50, min = 0)
#'  ObsGCA    <- seq(1, 0.8, -0.2/314)
#'
#' ## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
#' DebrisCovGlac <- SnowGlacier_HBV(model = 3, inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
#'                                  initCond = c(10, 3, 1), param = c(1, 1, 0, 3, 1, 6))
#'
#' ## Soil routine
#' ObsSoCA     <- 1 - ObsGCA
#' inputMatrix <- cbind(DebrisCovGlac[ , 9], potEvap, ObsSoCA)
#'
#' soil <- Soil_HBV(model = 2, inputData = inputMatrix, initCond = c(50), param = c(200, 0.5, 2))
#'
#' @export
#'
#'
Soil_HBV <- function(model, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_Soil_HBV`, model, inputData, initCond, param)
}

#' @name Temp_model
#'
#' @title Altitude gradient base air temperature models
#'
#' @description Extrapolate air temperature records to another heights. In this package
#' version you can use the classical linear gradient model or a modified version which
#' sets an upper altitudinal threshold air temperature decrement (avoiding unreliable estimations).
#'
#' @usage Temp_model(
#'        model,
#'        inputData,
#'        zmeteo,
#'        ztopo,
#'        param
#' )
#' @param model numeric value with model option:
#' \itemize{
#'   \item 1: linear air temperature gradient (LT).
#'   \item 2: linear air temperature gradient with an upper threshold (LTM).
#' }
#'
#' @param inputData numeric vector with air temperature record series [ºC/\eqn{\Delta t}].
#'
#' @param zmeteo numeric value indicating the altitude where the air temperature is recorded
#' \eqn{[masl]}.
#'
#' @param ztopo numeric value with the target height \eqn{[masl]}.
#'
#' @param param numeric vector with the following parameters:
#'
#' \strong{LT}
#' \itemize{
#'   \item 1: air temperature linear gradient (\code{grad_t}) [ºC/km].
#' }
#'
#' \strong{LPM}
#' \itemize{
#'   \item 1: air temperature linear gradient (\code{grad_t}) [ºC/km].
#'   \item 2: threshold height. Air temperature does not decrease when the altitude (\code{ztopo})
#'   is higher than this value \eqn{[masl]}.
#' }
#'
#' @return Numeric vector with the extrapolated air temperature series.
#'
#' @references
#'
#' Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014.
#' The importance of observed gradients of air temperature and precipitation for modeling
#' runoff from a glacierized watershed in the Nepalese Himalayas.
#' Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## simple linear model
#' airTemp <- Temp_model(
#'                       model = 1,
#'                       inputData = runif(200, max = 25, min = -10),
#'                       zmeteo = 2000, ztopo = 3500, param = c(-6.5)
#'                       )
#'
#' @export
#'
#'
Temp_model <- function(model, inputData, zmeteo, ztopo, param) {
    .Call(`_HBV_IANIGLA_Temp_model`, model, inputData, zmeteo, ztopo, param)
}

#' @name UH
#'
#' @title Transfer function
#'
#' @description Use a triangular transfer function to adjust the timing of the
#' simulated streamflow discharge. This module represents the runoff routing in
#' the streams.
#'
#' @usage UH(
#'   model,
#'   Qg,
#'   param
#'   )
#'
#' @param model numeric integer with the transfer function model. The current HBV.IANIGLA
#' model only allows for a single option.
#' \itemize{
#'   \item 1: triangular function with a static base.
#' }
#'
#' @param Qg numeric vector with the water that gets into the stream.
#' If you are not modeling glaciers is the output of the
#' \code{\link{Routing_HBV}} module, otherwise, is the sum of the \code{\link{Routing_HBV}}
#' output plus the glacier discharge coming from the \code{\link{Glacier_Disch}} module.
#'
#' @param param numeric vector with the following values,
#'
#' \strong{Model 1}
#' \itemize{
#'   \item \code{Bmax}: base of the transfer function triangle \eqn{[timestep]}.
#' }
#'
#' @return Numeric vector with the simulated streamflow discharge.
#'
#' @references
#' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
#' modelling—experience from the HBV approach. Hydrol. Process.
#' 29, 3535–3545. https://doi.org/10.1002/hyp.10510
#'
#' Parajka, J., Merz, R., & Blöschl, G. (2007). Uncertainty and multiple objective
#' calibration in regional water balance modelling: Case study in 320 Austrian catchments.
#' Hydrological Processes, 21(4), 435-446. https://doi.org/10.1002/hyp.6253
#'
#' @examples
#' # The following is a toy example. I strongly recommend to see
#' # the package vignettes in order to improve your skills on HBV.IANIGLA
#'
#' ## Routing example
#' inputMatrix <- cbind(runif(n = 200, max = 100, min = 0), runif(n = 200, max = 50, min = 5),
#'                  runif(n = 100, max = 3, min = 1))
#'
#' routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
#'                          initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))
#'
#' ## UH
#' dischOut <- UH(model = 1, Qg = routeMod1[ , 1], param = 2.2)
#'
#' @export
#'
UH <- function(model, Qg, param) {
    .Call(`_HBV_IANIGLA_UH`, model, Qg, param)
}

icemelt_clean <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_icemelt_clean`, inputData, initCond, param)
}

icemelt_clean_gca <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_icemelt_clean_gca`, inputData, initCond, param)
}

icemelt_debris <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_icemelt_debris`, inputData, initCond, param)
}

icemelt_debris_gca <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_icemelt_debris_gca`, inputData, initCond, param)
}

route_1r_2o <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_route_1r_2o`, inputData, initCond, param)
}

route_1r_3o <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_route_1r_3o`, inputData, initCond, param)
}

route_2r_2o <- function(lake, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_route_2r_2o`, lake, inputData, initCond, param)
}

route_2r_3o <- function(lake, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_route_2r_3o`, lake, inputData, initCond, param)
}

route_3r_3o <- function(lake, inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_route_3r_3o`, lake, inputData, initCond, param)
}

snowmelt <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_snowmelt`, inputData, initCond, param)
}

snowmelt_sca <- function(inputData, initCond, param) {
    .Call(`_HBV_IANIGLA_snowmelt_sca`, inputData, initCond, param)
}

medianCpp <- function(x) {
    .Call(`_HBV_IANIGLA_medianCpp`, x)
}

