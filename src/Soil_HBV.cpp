#include <Rcpp.h>
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

//NOTA 1: este es un modelo que sirve para simular abstracciones, condiciones antecedentes de humedad,
// pérdidas por evaporación o evapotranspiración y calcular flujo de agua que entra a los reservorios.
//NOTA 2: el desarrollo de esta rutina está en consonancia con la idea de darle al usuario la posibilidad
// de trabajar con un modelo del tipo ESMA en versión semidistribuída. El paradigma de la modelación al que
// responde este tipo de rutina (junto con la de los reservorios) es la de dinámica de sistemas.

// MODELO - model
// Entero con el modelo a correr
// #1# Sin variación en el área de suelo
// #2# Con área de suelo variable -> se acopla al modelo 3 de la rutina nivo-glaciar

// DATOS DE ENTRADA - inputData
// Matriz con las siguientes columnas

  // Modelo 1
// #1# Total: serie de (Mice + Msnow + Prain) [mm/deltaT]
// #2# PET  : evapotranspiración potencial [mm/deltaT]*
// *depende de si se trata de suelo/roca pelados, con algún tipo de vegetación o bosque

  // Modelo 2
// #1# Total: serie de (Mice + Msnow + Prain) [mm/deltaT]
// #2# PET  : evapotranspiración potencial [mm/deltaT]*
// #3# SoCA*: serie con valores de área respecto al total de la cuenca
// *depende de si se trata de suelo/roca pelados, con algún tipo de vegetación o bosque
// *SoCA: Soil Covered Area

// CONDICIONES INICIALES - initCond
// Vector con los siguientes datos
// #1# SM       : contenido de humedad/agua inicial en el suelo [mm]
// #2# relatArea: área relativa de la celda

// PARÁMTEROS A CALIBRAR - param
// Vector de parámetros
// #1# FC  : capacidad de campo ficticia [mm]
// #2# LP  : factor de ajuste para cálculo de evaporación/evapotranspiración actual [-]
// #3# beta: exponente que permite relacionar de manera no lineal entrada efectiva con entrada de agua total [-]

// SALIDA
// La matriz de salida tiene las siguientes columnas
// #1# Rech: vector con valores de recarga que van a los reservorios subterráneos [mm/deltaT]
// #2# Eact: vector con valores de evaporación/evapotranspiración actual [mm/deltaT]
// #3# SM  : vector con valores de contenido de humedad de la celda [mm/deltaT]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name Soil_HBV
//'
//' @title Empirical soil moisture routine
//'
//' @description This module allows you to account for actual evapotranspiration,
//' abstractions, antecedent conditions and effective runoff. The formulation enables
//' non linear relationships between soil box water input (rainfall plus snowmelt) and
//' the effective runoff. This effective value is the input series to the routine function
//' (\code{\link{Routing_HBV}}).
//'
//' @usage Soil_HBV(
//'        model,
//'        inputData,
//'        initCond,
//'        param
//'        )
//'
//' @param model numeric integer suggesting one of the following options:
//' \itemize{
//'   \item 1: Classical HBV soil moisture routine.
//'   \item 2: HBV soil moisture routine with varying area. This option should
//'   be used with \code{\link{SnowGlacier_HBV}}'s \strong{\emph{model 3}}.
//' }
//'
//' @param inputData numeric matrix with the following series
//'
//' \strong{Model 1}
//' \itemize{
//'   \item \code{column_1}: \code{Total = Prain + Msnow} \eqn{[mm/\Delta t]}. This
//'   series comes from the output of the \code{\link{SnowGlacier_HBV}} module.
//'   \item \code{column_2}: potential evapotranspiration  \eqn{[mm/\Delta t]}. Since
//'   the package has a simple model (\code{\link{PET}}) to obtain this
//'   series I strongly recommend using the
//'   \href{https://CRAN.R-project.org/package=Evapotranspiration}{\code{Evapotranspiration}}
//'   package.
//' }
//'
//' \strong{Model 2}
//' \itemize{
//'   \item \code{column_1}: as in \strong{model 1}.
//'   \item \code{column_2}: as in \strong{model 1}.
//'   \item \code{column_3} : relative soil area (ratio of soil surface over
//'   basin area). When the glacier area changes the soil does the same, so coherence
//'   between this two series should be seek.This value is used to scale the effective
//'   runoff accordingly (\code{Rech} column in the matrix output).
//' }
//'
//' @param initCond numeric vector with the following values:
//'  \enumerate{
//'   \item initial soil water content \eqn{[mm]}. This is a model state variable
//'   and is internally used as first soil moisture value.
//'   \item relative area \eqn{[-]}. Only needed when using \strong{\code{model 1}}.
//'   This is the soil surface proportion relative to the catchment as a whole, so
//'   the values should never supersede one (1). This value is used to scale the effective
//'   runoff accordingly (\code{Rech} column in the matrix output).
//'}
//'
//' @param param numeric vector with the following values:
//' \enumerate{
//'   \item \code{FC}: fictitious soil field capacity \eqn{[mm]}.
//'   \item \code{LP}: parameter to get actual ET \eqn{[-]}.
//'   \item \eqn{\beta}: exponential value that allows for non-linear relations between
//'   soil box water input (rainfall plus snowmelt) and the effective runoff \eqn{[-]}.
//' }
//'
//' @return Numeric matrix with the following columns:
//' \enumerate{
//'   \item \code{Rech}: recharge series \eqn{[mm/\Delta t]}. This is the input to
//'   the \code{\link{Routing_HBV}} module.
//'   \item \code{Eact}: actual evapotranspiration series \eqn{[mm/\Delta t]}.
//'   \item \code{SM}: soil moisture series \eqn{[mm/\Delta t]}.
//' }
//'
//' @references
//' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
//' modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
//' https://doi.org/10.1002/hyp.10510
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' # HBV soil routine with variable area
//' ## Calder's model
//' potEvap <- PET(model = 1, hemis = 1, inputData = as.matrix(1:315), elev = c(1000, 1500),
//'               param = c(4, 0.5))
//'
//' ## Debris-covered ice
//'  ObsTemp   <- sin(x = seq(0, 10*pi, 0.1))
//'  ObsPrecip <- runif(n = 315, max = 50, min = 0)
//'  ObsGCA    <- seq(1, 0.8, -0.2/314)
//'
//' ## Fine debris covered layer assumed. Note that the ice-melt factor is cumpulsory but harmless.
//' DebrisCovGlac <- SnowGlacier_HBV(model = 3, inputData = cbind(ObsTemp, ObsPrecip, ObsGCA),
//'                                  initCond = c(10, 3, 1), param = c(1, 1, 0, 3, 1, 6))
//'
//' ## Soil routine
//' ObsSoCA     <- 1 - ObsGCA
//' inputMatrix <- cbind(DebrisCovGlac[ , 9], potEvap, ObsSoCA)
//'
//' soil <- Soil_HBV(model = 2, inputData = inputMatrix, initCond = c(50), param = c(200, 0.5, 2))
//'
//' @export
//'
//'
// [[Rcpp::export]]
NumericVector Soil_HBV(int model,
                       NumericMatrix inputData,
                       NumericVector initCond,
                       NumericVector param) {
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

  int n = inputData.nrow(); // número de filas
  int m = 3; //número de columnas

  // MODELO //
  if (model == 1) {
    // MODELO CLÁSICO

    // verifico condiciones
    if (inputData.ncol() < 2) {
      stop("Please verify the inputData matrix");
    }
    if (initCond.size() < 2) {
      stop("Please verify the initCond vector");
    }
    if (param.size() < 3) {
      stop("Please verify the param vector");
    }
    if (param[0] <= 0) {
      stop("Verify: FC > 0");
    }
    if ( (param[1] > 1) || (param[1] <= 0) ) {
      stop("Verify: 0 < LP <= 1");
    }

    // defino variables, parámetros y matriz de salida
    double Eac, Ieff, Def, SM; // variables intermedias y finales
    double FC, LP, beta;       // parámetros
    NumericMatrix out(n, m);   // matriz de salida

    // Le doy valores a los parámetros
    FC   = param[0];
    LP   = param[1];
    beta = param[2];

    for (int i = 0; i < n; ++i) {
      if (i == 0) { // primera corrida

        // me aseguro que el usuario no cometa el error de colocar SM incial > FC
        if (initCond[0] <= param[0]){
          SM = initCond[0];
        } else {
          SM = param[0];
        }

        Eac  = inputData(i, 1) * std::min(SM / (FC * LP), 1.0);
        Ieff = inputData(i, 0) * std::pow( (SM / FC), beta );
        Def  = SM + inputData(i, 0) - Ieff - Eac;

        // me aseguro que cierre el balance de masa
        if (Def < 0.0) {
          Eac = SM;
          SM  = 0.0;

        } else if (Def <= FC){
          SM  = Def;

        } else {//caso en que la humedad del suelo es mayor a FC
          Ieff = Ieff + (Def - FC);
          SM   = FC;
        }

      } else {
        Eac  = inputData(i, 1) * std::min(SM / (FC * LP), 1.0);
        Ieff = inputData(i, 0) * std::pow( (SM / FC), beta );
        Def  = SM + inputData(i, 0) - Ieff - Eac;

        // me aseguro que cierre el balance de masa
        if (Def < 0.0) {
          Eac = SM;
          SM  = 0.0;

        } else if (Def <= FC){
          SM  = Def;

        } else { //caso en que la humedad del suelo es mayor a FC
          Ieff = Ieff + (Def - FC);
          SM   = FC;
        }
      }

      // Armo matriz de salida
      out(i, 0) = Ieff * initCond[1]; //Recarga
      out(i, 1) = Eac;
      out(i, 2) = SM;

    }

    colnames(out) = CharacterVector::create("Rech", "Eac", "SM");
    return out;

  } else if (model == 2) {
    // MODELO CON ÁREA VARIABLE

    // verifico condiciones
    if (inputData.ncol() < 3) {
      stop("Please verify the inputData matrix");
    }
    if (initCond.size() < 1) {
      stop("Please verify the initCond vector");
    }
    if (param.size() < 3) {
      stop("Please verify the param vector");
    }
    if (param[0] <= 0) {
      stop("Verify: FC > 0");
    }
    if ( (param[1] > 1) || (param[1] <= 0) ) {
      stop("Verify: 0 < LP <= 1");
    }

    // defino variables, parámetros y matriz de salida
    double Eac, Ieff, Def, SM; // variables intermedias y finales
    double FC, LP, beta;       // parámetros
    NumericMatrix out(n, m);   // matriz de salida

    // Le doy valores a los parámetros
    FC   = param[0];
    LP   = param[1];
    beta = param[2];

    for (int i = 0; i < n; ++i) {
      if (i == 0) { // primera corrida

        // me aseguro que el usuario no cometa el error de colocar SM incial > FC
        if (initCond[0] <= param[0]){
          SM = initCond[0];
        } else {
          SM = param[0];
        }

        Eac  = inputData(i, 1) * std::min(SM / (FC * LP), 1.0);
        Ieff = inputData(i, 0) * std::pow( (SM / FC), beta );
        Def  = SM + inputData(i, 0) - Ieff - Eac;

        // me aseguro que cierre el balance de masa
        if (Def < 0.0) {
          Eac = SM;
          SM  = 0.0;

        } else if (Def <= FC){
          SM  = Def;

        } else {//caso en que la humedad del suelo es mayor a FC
          Ieff = Ieff + (Def - FC);
          SM   = FC;
        }

      } else {
        Eac  = inputData(i, 1) * std::min(SM / (FC * LP), 1.0);
        Ieff = inputData(i, 0) * std::pow( (SM / FC), beta );
        Def  = SM + inputData(i, 0) - Ieff - Eac;

        // me aseguro que cierre el balance de masa
        if (Def < 0.0) {
          Eac = SM;
          SM  = 0.0;

        } else if (Def <= FC){
          SM  = Def;

        } else { //caso en que la humedad del suelo es mayor a FC
          Ieff = Ieff + (Def - FC);
          SM   = FC;
        }
      }

      // Armo matriz de salida
      out(i, 0) = Ieff * inputData(i, 2); //Recarga
      out(i, 1) = Eac;
      out(i, 2) = SM;

    }

    colnames(out) = CharacterVector::create("Rech", "Eac", "SM");
    return out;


  } else {
    stop("Model not available");
  }


}
