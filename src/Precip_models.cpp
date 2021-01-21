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
  // #model:     (1) gradiente lineal de P
  //             (2) gradiente lineal de P más tope de aumento

// DATOS DE ENTRADA - inputData
  // Modelo GRADIENTE LINEAL Precipitación
  // #precip:   serie de precipitaciones [mm/deltaT]

  // Modelo GRADIENTE LINEAL Precipitación más tope de aumento precipitación
  // #precip:   serie de precipitaciones [mm/deltaT]

// DATOS DE TOPOGRAFÍA METEROROLÓGICOS - zmeteo
  // zprecip: altura a la que se encuentra el pluviómetro [msnm]

// DATOS DE TOPOGRAFÍA - ztopo
  // zcell:   altura a la que se enceuntra la celda [msnm]

//PARÁMTEROS A CALIBRAR - param
  // Modelo GRADIENTE LINEAL Precipitación
  // #1#gradP: gradiente lineal de precipitación [%/100m]

  // Modelo GRADIENTE LINEAL Precipitación más tope altura
  // #1#gradP: gradiente lineal de precipitación [%/100m]
  // #1#maxALT: altura por sobre la cuál el no hay más aumento de precipitación [msnm]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name Precip_model
//'
//' @title Altitude gradient based precipitation models
//'
//' @description Extrapolate precipitation gauge measurements to another heights. In this package
//' version you can use the classical linear gradient model or a modified version which
//' sets a threshold altitude for precipitation increment (avoiding unreliable estimations).
//'
//' @usage Precip_model(
//'        model,
//'        inputData,
//'        zmeteo,
//'        ztopo,
//'        param
//' )
//'
//' @param model numeric value with model option:
//' \itemize{
//'   \item 1: linear precipitation gradient (LP).
//'   \item 2: linear precipitation gradient with an upper threshold (LPM).
//' }
//'
//' @param inputData numeric vector with precipitation gauge series \eqn{[mm/\Delta t]}.
//'
//' @param zmeteo numeric value indicating the altitude of the precipitation gauge \eqn{[masl]}.
//'
//' @param ztopo numeric value with the target height \eqn{[masl]}.
//'
//' @param param numeric vector with the following parameters:
//'
//' \strong{LP}
//' \itemize{
//'   \item 1: precipitation gradient (\code{gradP}) [\%/100m].
//' }
//'
//' \strong{LPM}
//' \itemize{
//'   \item 1: precipitation gradient (\code{gradP}) [\%/100m].
//'   \item 2: threshold height. Precipitation does not increase when the altitude (\code{ztopo})
//'   is higher than this parameter \eqn{[masl]}.
//' }
//'
//' @return Numeric vector with the extrapolated precipitation series.
//'
//' @references For some interesting work on precipitation gradients at catchment and
//' synoptic scale see:
//'
//' Immerzeel, W.W., Petersen, L., Ragettli, S., Pellicciotti, F., 2014.
//' The importance of observed gradients of air temperature and precipitation for modeling
//' runoff from a glacierized watershed in the Nepalese Himalayas.
//' Water Resour. Res. 50, 2212–2226. https://doi.org/10.1002/2013WR014506
//'
//' Viale, M., Nuñez, M.N., 2010. Climatology of Winter Orographic Precipitation over the
//' Subtropical Central Andes and Associated Synoptic and Regional Characteristics.
//' J. Hydrometeorol. 12, 481–507. https://doi.org/10.1175/2010JHM1284.1
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//'## LP case
//' set.seed(369)
//'
//' precLP <- Precip_model(model = 1, inputData = runif(n = 365, max = 30, min = 0),
//'                         zmeteo = 3000, ztopo = 4700, param = c(5))
//'
//'## LPM case
//' set.seed(369)
//'
//' precLPM <- Precip_model(model = 2, inputData = runif(n = 365, max = 30, min = 0),
//'                         zmeteo = 3000, ztopo = 4700, param = c(5, 4500))
//'
//' @export
//'
//'
// [[Rcpp::export]]
NumericVector Precip_model(int model,
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
    // MODELO CON GRADIENTES LINEAL DE PRECIPITACIÓN //

    int n = inputData.size();
    NumericVector out(n);
    double gradP;

    // Asigno variables
    gradP = param[0];

    for (int i = 0; i < n; ++i) {
      //serie de precipitación
      if (inputData[i] == 0.0) { //si no precipita
        out[i] = 0.0;
      } else {
        out[i] = std::max( (1 + (ztopo - zmeteo) * (gradP / (100 * 100) ) ) * inputData[i], 0.0);
      }
    } // Fin loop for

    return out;

  } else if (model == 2) {
    // MODELO CON GRADIENTE LINEAL + TOPE DE ALTURA //

    int n = inputData.size();
    NumericVector out(n);
    double gradP, maxALT;

    // Asigno variables
    gradP  = param[0];
    maxALT = param[1];

    for (int i = 0; i < n; ++i) {
      if (inputData[i] == 0.0) { //si no precipita
        out[i] = 0.0;

      } else {
        if (ztopo <= maxALT) {
          out[i] = std::max(  (1 + (ztopo - zmeteo) * (gradP / (100 * 100) ) ) * inputData[i], 0.0);

        } else {
          out[i] = std::max(  (1 + (maxALT - zmeteo) * (gradP / (100 * 100) ) ) * inputData[i], 0.0);

        }

      }


    }//Fin loop for

    return out;

  } else {
    stop("Model not available");

  }

}
