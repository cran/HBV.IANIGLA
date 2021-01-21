#include <Rcpp.h>
#include "median.h"
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
// FUNCIÓN DE TRANSFERENCIA - model
// #1# Función de transferencia triangular estática

// DATOS DE ENTRADA - inputData
// #1# Qg: salida total de los reservorios

// PARÁMETROS - param
// Modelo 1: HU triangular estático
// #1# Bmax: máximo retraso. Base del HU triangular

// SALIDA - output
// Modelo 1: HU triangular estático
// #1# Q: hidrograma de salida de la cuenca/subcuenca

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
 */

//' @name UH
//'
//' @title Transfer function
//'
//' @description Use a triangular transfer function to adjust the timing of the
//' simulated streamflow discharge. This module represents the runoff routing in
//' the streams.
//'
//' @usage UH(
//'   model,
//'   Qg,
//'   param
//'   )
//'
//' @param model numeric integer with the transfer function model. The current HBV.IANIGLA
//' model only allows for a single option.
//' \itemize{
//'   \item 1: triangular function with a static base.
//' }
//'
//' @param Qg numeric vector with the water that gets into the stream.
//' If you are not modeling glaciers is the output of the
//' \code{\link{Routing_HBV}} module, otherwise, is the sum of the \code{\link{Routing_HBV}}
//' output plus the glacier discharge coming from the \code{\link{Glacier_Disch}} module.
//'
//' @param param numeric vector with the following values,
//'
//' \strong{Model 1}
//' \itemize{
//'   \item \code{Bmax}: base of the transfer function triangle \eqn{[timestep]}.
//' }
//'
//' @return Numeric vector with the simulated streamflow discharge.
//'
//' @references
//' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in hydrological
//' modelling—experience from the HBV approach. Hydrol. Process.
//' 29, 3535–3545. https://doi.org/10.1002/hyp.10510
//'
//' Parajka, J., Merz, R., & Blöschl, G. (2007). Uncertainty and multiple objective
//' calibration in regional water balance modelling: Case study in 320 Austrian catchments.
//' Hydrological Processes, 21(4), 435-446. https://doi.org/10.1002/hyp.6253
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## Routing example
//' inputMatrix <- cbind(runif(n = 200, max = 100, min = 0), runif(n = 200, max = 50, min = 5),
//'                  runif(n = 100, max = 3, min = 1))
//'
//' routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
//'                          initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))
//'
//' ## UH
//' dischOut <- UH(model = 1, Qg = routeMod1[ , 1], param = 2.2)
//'
//' @export
//'
// [[Rcpp::export]]
NumericVector UH(int model,
                 NumericVector Qg,
                 NumericVector param){
  if (model == 1){
    // HU TRIANGULAR ESTÁTICO //

  // Le doy nombre a parámetro
  double Bmax = param[0];

  // Verifico que Bmax sea mayor o igual a 1
  if (Bmax < 1){
    stop("Parameter must be Bmax >= 1");
  }
  // Defino tamaño de vectores
  double n = ceil(Bmax); // intervalos en que divido el HU
  int m = Qg.size();  // tamaño del vector de salida
  int j;              // contador que uso para un loop

  // Defino variables y salida
  double hm, eps, Tp;  // HU de base uno
  NumericVector t(n), h(n), w(n);
  NumericVector out(m);

  // Doy valores
  hm  = 2.0;
  eps = 4.0;
  Tp  = 0.5;


  if (n == 1) {
    out = Qg;
    return out;

  } else {
    t[0] = 1 / Bmax;
    h[0] = hm - std::abs(t[0] - Tp) * eps;

    for (int i = 1; i < (n - 1); ++i) {
      t[i] = t[i - 1] + 1 / Bmax;
      h[i] = hm - std::abs(t[i] - Tp) * eps;
    }

    t[n - 1] = 1.0;
    h[n - 1] = 0.0;

    // Cálculo de ponderadores
    if (Bmax < 2) {
      w[0] = 0.5 + (hm + h[0]) * (t[0] - Tp) * 0.5;
      w[1] = 1 - w[0];

    } else {
      w[0] = t[0] * h[0] * 0.5;
      j    = medianCpp(n) - 1;
      for (int i = 1; i < n; ++i) {
       if (i == j) {
         w[i] = (h[i - 1] + hm) * (Tp - t[i -1]) * 0.5 + (h[i] + hm) * (t[i] - Tp) * 0.5;

       } else {
         w[i] = (h[i] + h[i - 1]) * (t[i] - t[i - 1]) * 0.5;

       }
      } // cierre loop for()

    }

    // Cálculo del hidrograma de salida
    for (int i = 0; i < m; ++i) {
      double Qf = 0.0; // variable ficticia

      for (int j = 0; j < n; ++j) {
        Qf += Qg[i - j] * w[j];

      }

      out[i] = Qf;



      }


    return out;
  }

  } else {
    stop("Model not available");
  }
}
