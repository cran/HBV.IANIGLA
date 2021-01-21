#include <Rcpp.h>
#include "aa_snowmelt.h"
#include "aa_icemelt_debris.h"
#include "aa_icemelt_clean.h"
#include "aa_snowmelt_sca.h"
#include "aa_icemelt_clean_gca.h"
#include "aa_icemelt_debris_gca.h"
using namespace Rcpp;

// **********************************************************
//  Author       : Ezequiel Toum
//  Licence      : GPL V3
//  Institution  : IANIGLA-CONICET
//  e-mail       : etoum@mendoza-conicet.gob.ar
//  **********************************************************
//  HBV.IANIGLA package is distributed in the hope that it
//  will be useful but WITHOUT ANY WARRANTY.
//  **********************************************************

/*
// some spanish notes about the model
// ELECCIÓN DEL MODELO QUE VOY A CORRER - model
// #model:     (1) modelo TI ; (2) modelo TI con SCA ; (3) modelo TI con área glaciar variable

// DATOS DE ENTRADA - inputData
  // Modelo TI
// #1# airT:     serie de temperatura máxima [°C/deltaT]
// #2# precip:   serie de precipitaciones [mm/deltaT]

  // Modelo TI con SCA
// #1# airT:     serie de temperatura máxima [°C/deltaT]
// #2# precip:   serie de precipitaciones [mm/deltaT]
// #3# SCA:      serie de cobertura nívea [0 ; 1] [-]

  // Modelo TI con área glaciar variable
// #1# airT:   serie de temperatura máxima [°C/deltaT]
// #2# precip: serie de precipitaciones [mm/deltaT]
// #3# GCA*:   serie de cobertura glaciar relativo a toda la cuenca [-]

  // *GCA:  Glacier Covered Area

// CONDICIONES INICIALES - initCond
// #1# initCond:  valor inicial del equivalente agua nieve [mm]
// #2# initCond:  tipo de superficie debajo de la nieve (1)glaciar descubierto - (2)suelo - (3) glaciar cubierto
// #3# initCond:  area relativa de la zona o banda de elevación con respecto a la cuenca *
// *NOTA: sólo requerido cuando la superficie es glaciar en los modelos 1 y 2

//PARÁMTEROS A CALIBRAR - param
  // Modelos TI, TI con SCA y TI con serie GCA
// #1# SFCF: factor de corrección de caída de precipitación [-]
// #2# Tr  : temperatura que divide precipitación líquida de sólida [°C]
// #3# Tt  : temperatura umbral de derretimiento [°C]
// #4# fm  : factor de derretimiento para nieve [mm/°C.deltaT]
// #5# fi  : factor de derretimiento para hielo [mm/°C.deltaT]
// #6# fic : factor de derretimiento para hielo cubierto [mm/°C.deltaT]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name SnowGlacier_HBV
//'
//' @title Snow and ice-melt models
//'
//' @description Allows you to simulate snow accumulation and melting processes
//' using a temperature index approach. The function also incorporates options
//' for clean and debris covered glacier surface mass balance simulations.
//'
//' @usage SnowGlacier_HBV(
//'        model,
//'        inputData,
//'        initCond,
//'        param
//' )
//'
//' @param model numeric indicating which model you will use:
//' \itemize{
//'   \item 1: temperature index model.
//'   \item 2: temperature index model with a variable snow cover area as input data
//'   (as in the Snowmelt Runoff Model - SRM).
//'   \item 3: temperature index model with a variable glacier area as input data.
//' }
//'
//' @param inputData numeric matrix being columns the input variables. As in the whole
//' package functions, \code{NA_real_}  values are forbidden. When speaking about model
//' options we refer to the \strong{\code{model}} argument.
//'
//' \strong{Model 1:} \itemize{
//' \item \code{column_1}: air temperature series \eqn{[°C/\Delta t]}.
//' \item \code{column_2}: precipitation series \eqn{[mm/\Delta t]}.
//' }
//'
//' \strong{Model 2:} \itemize{
//' \item \code{column_1}: air temperature \eqn{[°C/\Delta t]}.
//' \item \code{column_2}: precipitation \eqn{[mm/\Delta t]}.
//' \item \code{column_3}: snow cover area. Values between [0 ; 1] \eqn{[-]}.
//' }
//'
//' \strong{Model 3:} \itemize{
//' \item \code{column_1}: air temperature  \eqn{[°C/\Delta t]}.
//' \item \code{column_2}: precipitation  \eqn{[mm/\Delta t]}.
//' \item \code{column_3}: glacier cover area. This area values are relative to the
//' total surface area of the basin \eqn{[-]}.
//' }
//'
//' @param initCond numeric vector with the following values.
//'  \itemize{
//'  \item \code{SWE0}: initial snow water equivalent \eqn{[mm]}.
//'  \item numeric integer indicating the surface type. \emph{1}: clean ice; \emph{2}: soil;
//'  \emph{3}: debris-covered ice.
//'  \item area of the glacier(s) (in the elevation band) relative to the basin; e.g.: 0.1 \eqn{[-]}.
//'  This option is required in \emph{Model 1} and \emph{Model 2} when surface is a glacier.
//'  }
//'
//' @param param numeric vector with the following values:
//'  \enumerate{
//'  \item \code{SFCF}: snowfall correction factor \eqn{[-]}.
//'  \item \code{Tr}: solid and liquid precipitation threshold temperature \eqn{[ºC]}.
//'  \item \code{Tt}: melt temperature \eqn{[ºC]}.
//'  \item \code{fm}: snowmelt factor \eqn{[mm/°C.\Delta t]}.
//'  \item \code{fi}: icemelt factor \eqn{[mm/°C.\Delta t]}.
//'  \item \code{fic}: debris-covered ice-melt factor \eqn{[mm/°C.\Delta t]}.
//'  }
//'
//' @return Numeric matrix with the following columns:
//'
//' \strong{Model 1}
//'
//' ** if surface is soil,
//' \enumerate{
//'   \item \code{Prain}: precip. as rainfall.
//'   \item \code{Psnow}: precip. as snowfall.
//'   \item \code{SWE}: snow water equivalent.
//'   \item \code{Msnow}: melted snow.
//'   \item \code{Total}: \code{Prain} + \code{Msnow}.
//' }
//'
//'  ** if surface is ice,
//'  \enumerate{
//'    \item \code{Prain}: precip. as rainfall.
//'    \item \code{Psnow}: precip. as snowfall.
//'    \item \code{SWE}: snow water equivalent.
//'    \item \code{Msnow}: melted snow.
//'    \item \code{Mice}: melted ice.
//'    \item \code{Mtot}: \code{Msnow} + \code{Mice}.
//'    \item \code{Cum}: \code{Psnow} - \code{Mtot}.
//'    \item \code{Total}: \code{Prain} + \code{Mtot}.
//'    \item \code{TotScal}: \code{Total} * initCond[3].
//'  }
//'
//' \strong{Model 2}
//'
//' ** if surface is soil,
//' \enumerate{
//'    \item \code{Prain}: precip. as rainfall.
//'    \item \code{Psnow}: precip. as snowfall.
//'    \item \code{SWE}: snow water equivalent.
//'    \item \code{Msnow}: melted snow.
//'    \item \code{Total}: \code{Prain} + \code{Msnow}.
//'    \item \code{TotScal}: \code{Msnow} * \code{SCA} + \code{Prain}.
//'  }
//'
//' ** if surface is ice -> as in \emph{Model 1}
//'
//' \strong{Model 3}
//'
//' ** if surface is soil -> as in \emph{Model 1}
//'
//' ** if surface is ice,
//'  \enumerate{
//'    \item \code{Prain}: precip. as rainfall.
//'    \item \code{Psnow}: precip. as snowfall.
//'    \item \code{SWE}: snow water equivalent.
//'    \item \code{Msnow}: melted snow.
//'    \item \code{Mice}: melted ice.
//'    \item \code{Mtot}: \code{Msnow} + \code{Mice}.
//'    \item \code{Cum}: \code{Psnow} - \code{Mtot}.
//'    \item \code{Total}: \code{Prain} + \code{Mtot}.
//'    \item \code{TotScal}: \code{Total} *  inputData[i, 3].
//'  }
//'
//' @references
//' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
//' modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
//' https://doi.org/10.1002/hyp.10510
//'
//' DeWalle, D. R., & Rango, A. (2008). Principles of Snow Hydrology.
//'
//' Parajka, J., Merz, R., Blöschl, G., 2007. Uncertainty and multiple objective calibration
//' in regional water balance modelling: case study in 320 Austrian catchments.
//' Hydrol. Process. 21, 435–446. https://doi.org/10.1002/hyp.6253
//'
//' Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly
//' catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325.
//' https://doi.org/10.5194/hess-16-3315-2012
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## Debris-covered ice
//' ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
//' ObsPrecip <- runif(n = 315, max = 50, min = 0)
//' ObsGCA    <- seq(1, 0.8, -0.2/314)
//'
//' ## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
//' DebrisCovGlac <- SnowGlacier_HBV(model = 3,
//'                                  inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
//'                                  initCond = c(10, 3, 1),
//'                                  param = c(1, 1, 0, 3, 1, 6))
//'
//' @export
//'
//'
// [[Rcpp::export]]
NumericMatrix SnowGlacier_HBV(int model,
                              NumericMatrix inputData,
                              NumericVector initCond,
                              NumericVector param){
  // *********************
  //  conditionals
  // *********************

  // check for NA_real_
  // inputData
  int chk_1 = sum( is_na(inputData) );
  if(chk_1 != 0){

    stop("inputData argument should not contain NA values!");

  }

  // initCond
  int chk_2 = sum( is_na(initCond) );
  if(chk_2 != 0){

    stop("initCond argument should not contain NA values!");

  }

  // param
  int chk_3 = sum( is_na(param) );
  if(chk_3 != 0){

    stop("param argument should not contain NA values!");

  }


  // *********************
  //  models
  // *********************
  if (model == 1) {
    // MODELO TI

    // GLACIAR DESCUBIERTO, SUELO O GLACIAR CUBIERTO

    if (initCond[1] == 1) {
      // SUPERFICIE GLACIAR

      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 3) {
        stop("You must support the relative area of the glacier");
      }
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = icemelt_clean(inputData = inputData,
                                        initCond = initCond,
                                        param = param);

      return(out);

    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO

      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      }
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = snowmelt(inputData = inputData,
                                   initCond = initCond,
                                   param = param);

      return(out);

    } else if (initCond[1] == 3) {

     // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS

     // Verifico condiciones
     if (inputData.ncol() < 2) {
       stop("Please verify the input matrix");
     }
     if (initCond.size() < 3) {
       stop("You must support the relative area of the glacier");
     }
     if (param.size() < 6){
       stop("Please verify the parameter vector");
     }

     NumericMatrix out = icemelt_debris(inputData = inputData,
                                        initCond = initCond,
                                        param = param);

     return(out);

    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }

  } else if (model == 2) {
    // MODELO TI CON SCA

    // GLACIAR DESCUBIERTO, SUELO O GLACIAR CUBIERTO

    if (initCond[1] == 1) {
      // SUPERFICIE GLACIAR

      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() == 2) {
        stop("You must support the relative area of the glacier");
      }
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = icemelt_clean(inputData = inputData,
                                        initCond = initCond,
                                        param = param);

      return(out);


    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO

      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      }
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = snowmelt_sca(inputData = inputData,
                                       initCond = initCond,
                                       param = param);

      return(out);

    } else if (initCond[1] == 3) {

      // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS

      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 3) {
        stop("You must support the relative area of the glacier");
      }
      if (param.size() < 6){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = icemelt_debris(inputData = inputData,
                                         initCond = initCond,
                                         param = param);

      return(out);

    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }

  } else if (model == 3) {
    // MODELO TI CON GCA

    // SUELO O GLACIAR

    if (initCond[1] == 1) {

      // SUPERFICIE GLACIAR

      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      }
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = icemelt_clean_gca(inputData = inputData,
                                            initCond = initCond,
                                            param = param);

      return(out);

    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO

      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      }
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = snowmelt(inputData = inputData,
                                   initCond = initCond,
                                   param = param);

      return(out);

    } else if (initCond[1] == 3) {

      // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS

      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      }
      if (param.size() < 6){
        stop("Please verify the parameter vector");
      }

      NumericMatrix out = icemelt_debris_gca(inputData = inputData,
                                             initCond = initCond,
                                             param = param);

      return(out);

    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }

  } else {
    stop("Model not avilable");
  }

} // FIN FUNCIÓN
