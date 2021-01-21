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
// #1# Sinusoidal -> Calder et al. (1983)

// HEMISFERIO EN EL QUE CORRO EL MODELO - hemis *
// #1# Hemisferio sur
// #2# Hemisferio norte
// *NOTA: sólo aplicable al modelo sinusoidal. En el resto de los modelos se debe proporcionar
// un valor pero el mismo no tiene efecto.

// DATOS DE ENTRADA - inputData
// Modelo sinusoidal
// #1# Julian dates: serie de días julianos para los cuales se debe calcular PET

// ELEVACIÓN - elev
// Modelo sinusoidal
// #1# zref: altura de referencia en la cuál conozco el valor aproximado de evapotranspiración
  // #2# ztopo: altura en la cuál me interesa calcular la PET

//PARÁMTEROS A CALIBRAR - param
// Modelo sinusoidal
// #1# PET: evapotranspiración potencial media diaria climatológica [mm]
// #2# gradPET: gradiente de disminución de evapotranspiración con altura [mm/100 m]

// SALIDA
// Modelo 1: Sinusoidal - Calder et al. (1983)
// Serie de evapotranspiración potencial diaria


//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name PET
//'
//' @title Potential evapotranspiration models
//'
//' @description Calculate your potential evapotranspiration series. This module was
//' design to provide a simple and straight forward way to calculate
//' one of the inputs for the soil routine (to show how does it works), but for real
//' world application I strongly recommend the use of the specialized
//' \href{https://CRAN.R-project.org/package=Evapotranspiration}{\code{Evapotranspiration}}
//' package.
//'
//' @usage PET(
//'   model,
//'   hemis,
//'   inputData,
//'   elev,
//'   param
//'   )
//'
//' @param model numeric value with model option:
//' \itemize{
//'   \item 1: Calder's model.
//' }
//'
//' @param hemis numeric value indicating the hemisphere:
//' \itemize{
//'   \item 1: southern hemisphere.
//'   \item 2: northern hemisphere.
//' }
//'
//' @param inputData numeric matrix with the following columns:
//'
//'  \strong{Calder's model}
//'  \itemize{
//'   \item \code{column_1}: julian dates, e.g: \code{as.matrix( c(1:365) )}.
//'  }
//'
//' @param elev numeric vector with the following values:
//'
//'  \strong{Calder's model}
//'  \itemize{
//'   \item 1: \code{zref}: the reference height where potential evapotranspiration or
//'   input data to calculate PET is known.
//'   \item 2: \code{ztopo}: target PET's topographic height.
//'  }
//'
//' @param param numeric vector with the following values:
//'
//' \strong{Calder's model}
//'  \itemize{
//'   \item 1: \code{PET}: climatological daily mean potential evapotranspiration [mm].
//'   \item 2: \code{gradPET}: evapotranspiration decrease gradient [mm/100 m].
//'  }
//'
//' @return Numeric vector with the potential evapotranspiration series.
//'
//' @references
//' Calder, I.R., Harding, R.J., Rosier, P.T.W., 1983. An objective assessment of soil-moisture
//' deficit models. J. Hydrol. 60, 329–355. https://doi.org/10.1016/0022-1694(83)90030-6
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## Run the model for a year in the southern hemisphere
//' potEvap <- PET(model = 1,
//'                hemis = 1,
//'                inputData = as.matrix(1:365),
//'                elev = c(1000, 1500),
//'                param = c(4, 0.5))
//'
//' @export
//'
// [[Rcpp::export]]
NumericVector PET(int model,
                  int hemis,
                  NumericMatrix inputData,
                  NumericVector elev,
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

  // elev
  int chk_2 = sum( is_na(elev) );
  if(chk_2 != 0){

    stop("elev argument should not contain NA values!");

  }

  // param
  int chk_3 = sum( is_na(param) );
  if(chk_3 != 0){

    stop("param argument should not contain NA values!");

  }


  // *********************
  //  function
  // *********************

  if (model == 1) {
    // SINUSOIDAL - Calder et al. (1983)

    if (hemis == 1) {
      // Hemisferio Sur
      int n = inputData.size();

      // Declaro variables
      double zref, ztopo;
      double EP, gradPET;
      NumericVector out(n);

      // Asigno valores a variables
      zref  = elev[0];
      ztopo = elev[1];

      EP      = param[0];
      gradPET = param[1];

      // Corro modelo
      for (int i = 0; i < n; ++i) {
        if (inputData(i, 0) == 0.0) { // PET igual a cero
          out[i] = 0.0;

        } else {

          out[i] = std::max(EP * ( 1 + std::sin( (360 * inputData[i] / 366 + 90) * M_PI / 180 ) ) +
            (ztopo -  zref) * (gradPET / 100), 0.0);

        }

      }

      return out;


    } else if (hemis == 2) {
      // Hemisferio Norte
      int n = inputData.size();

      // Declaro variables
      double zref, ztopo;
      double EP, gradPET;
      NumericVector out(n);

      // Asigno valores a variables
      zref  = elev[0];
      ztopo = elev[1];

      EP      = param[0];
      gradPET = param[1];

      // Corro modelo
      for (int i = 0; i < n; ++i) {
        if (inputData(i, 0) == 0.0) { // PET igual a cero
          out[i] = 0.0;

        } else {

          out[i] = std::max(EP * ( 1 + std::sin( (360 * inputData[i] / 366 - 90) * M_PI / 180 ) ) +
            (ztopo -  zref) * (gradPET / 100), 0.0);

        }

      }

      return out;

    } else {

      stop("Hemisphere must be 1 or 2");

    }

  } else {

    stop("Model not available");

  }
} // cierre función
