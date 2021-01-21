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
// ELECCIÓN DEL MODELO QUE VOY A CORRER - model
// #model:     (1) gradiente lineal de T

// DATOS DE ENTRADA - inputData
// Modelo GRADIENTE LINEAL de Temperatura
// #airT:     serie de temperatura máxima [°C/deltaT]

// DATOS DE TOPOGRAFÍA METEROROLÓGICOS - zmeteo
// #1# zmeteo:   altura a la que se encuentra el sensor de temp [msnm]

// DATOS DE TOPOGRAFÍA - ztopo
// #1# ztopo:   altura a la que se enceuntra la celda [msnm]

//PARÁMTEROS A CALIBRAR - param
// Modelo GRADIENTE LINEAL Temperatura
// #1#gradT: gradiente lineal de temperatura [°C/100m]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name Temp_model
//'
//' @title Altitude gradient base air temperature models
//'
//' @description Extrapolate air temperature records to another heights. In this package
//' version you can use the classical linear gradient model or a modified version which
//' sets an upper altitudinal threshold air temperature decrement (avoiding unreliable estimations).
//'
//' @usage Temp_model(
//'        model,
//'        inputData,
//'        zmeteo,
//'        ztopo,
//'        param
//' )
//' @param model numeric value with model option:
//' \itemize{
//'   \item 1: linear air temperature gradient (LT).
//'   \item 2: linear air temperature gradient with an upper threshold (LTM).
//' }
//'
//' @param inputData numeric vector with air temperature record series [ºC/\eqn{\Delta t}].
//'
//' @param zmeteo numeric value indicating the altitude where the air temperature is recorded
//' \eqn{[masl]}.
//'
//' @param ztopo numeric value with the target height \eqn{[masl]}.
//'
//' @param param numeric vector with the following parameters:
//'
//' \strong{LT}
//' \itemize{
//'   \item 1: air temperature linear gradient (\code{grad_t}) [ºC/km].
//' }
//'
//' \strong{LPM}
//' \itemize{
//'   \item 1: air temperature linear gradient (\code{grad_t}) [ºC/km].
//'   \item 2: threshold height. Air temperature does not decrease when the altitude (\code{ztopo})
//'   is higher than this value \eqn{[masl]}.
//' }
//'
//' @return Numeric vector with the extrapolated air temperature series.
//'
//' @references
//'
//' Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014.
//' The importance of observed gradients of air temperature and precipitation for modeling
//' runoff from a glacierized watershed in the Nepalese Himalayas.
//' Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## simple linear model
//' airTemp <- Temp_model(
//'                       model = 1,
//'                       inputData = runif(200, max = 25, min = -10),
//'                       zmeteo = 2000, ztopo = 3500, param = c(-6.5)
//'                       )
//'
//' @export
//'
//'
// [[Rcpp::export]]
NumericVector Temp_model(int model,
                         NumericVector inputData,
                         double zmeteo,
                         double ztopo,
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


  // param
  int chk_3 = sum( is_na(param) );
  if(chk_3 != 0){

    stop("param argument should not contain NA values!");

  }


  // *********************
  //  function
  // *********************

  if (model == 1){
    // MODELO CON GRADIENTE LINEAL //

    int n = inputData.size();
    NumericVector out(n);
    double gradT = param[0];

    for (int i = 0; i < n; ++i) {
      //serie de temperaturas
      out[i] = (ztopo - zmeteo) * (gradT / 1000) + inputData[i];

    }

    return out;

  } else if(model == 2){
    // linear gradient model with upper altitude threshold

    int n = inputData.size();
    NumericVector out(n);
    double gradT = param[0];
    double thres = param[1]; // altitudinal threshold

    for (int i = 0; i < n; ++i) {
      if(ztopo < thres){
        //serie de temperaturas
        out[i] = (ztopo - zmeteo) * (gradT / 1000) + inputData[i];

      } else {
        out[i] = (ztopo - thres) * (gradT / 1000) + inputData[i];
      }


    }

    return out;


  } else {
    stop("Model not available");

  }

}
