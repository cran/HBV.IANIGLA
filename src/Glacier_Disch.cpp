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
// #model:     (1) Stahl et al. 2008 (en esta versión AG es denominador)

// DATOS DE ENTRADA - inputData
  // Modelo Stahl et al. 2008
// #SWE:     serie equivalente agua nieve [mm/deltaT]
// #Total:   agua de deshielo más agua de lluvia [mm/deltaT]

// CONDICIONES INICIALES - initCond
  // Modelo Stahl et al. 2008
// #1# SG:  almacenamiento del reservorio glaciar inicial [mm]

// PARÁMTEROS A CALIBRAR - param
  // Modelo Stahl et al. 2008
// #1# KGmin: tasa de salida de agua mínima [1/deltaT]
// #2# dKG: aumento máximo de la tasa de salida [1/deltaT]
// #3# AG: factor de escala [mm]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!
*/

//' @name Glacier_Disch
//'
//' @title Glacier discharge conceptual model
//'
//' @description Implement the conceptual water storage and release formulation for
//' glacier runoff routing. The current model version follows the approach proposed
//' by \cite{Stahl et al. (2008)} (hereafter S08) for the Bridge River basin. Note that
//' the bucket storage and release concepts for glacier runoff modeling are also
//' described in \cite{Jansson et al. (2002)}.
//'
//' @usage Glacier_Disch(
//'        model,
//'        inputData,
//'        initCond,
//'        param
//'        )
//'
//' @param model numeric integer with the model's choice. The current HBV.IANIGLA version
//' only supports the \strong{S08} approach.
//'  \itemize{
//'   \item 1:\strong{S08} glacier storage and release model.
//'   }
//'
//'   \if{html}{\figure{glacier_discharge_hbv.png}{options: width=200}}
//'
//' @param inputData numeric matrix with two columns:
//'
//'  \strong{Model 1}
//'  \itemize{
//'  \item \code{column_1}: snow water equivalent above the glacier \eqn{[mm/\Delta t]}.
//'  The series can be obtained from the \link{SnowGlacier_HBV} function output.
//'  \item \code{column_2}:  melted snow + melted ice + rainfall \eqn{[mm/\Delta t]}. This
//'  series comes from the \strong{TotScal} column in the \link{SnowGlacier_HBV}
//'  function output.
//'  }
//'
//' @param initCond numeric value with the initial glacier reservoir
//' water content \strong{\code{SG}} \eqn{[mm]}.
//'
//' @param param numeric vector with the following values:
//'
//' \strong{Model 1 (S08)}
//'  \itemize{
//'  \item \code{KGmin}: minimal outflow rate \eqn{[1/\Delta t]}.
//'  \item \code{dKG}:  maximum outflow rate increase \eqn{[1/\Delta t]}.
//'  \item \code{AG}: scale factor \eqn{[mm]}.
//'  }
//'
//' @return Numeric matrix with the following columns:
//'
//' \strong{Model 1 (S08)}
//' \itemize{
//'   \item \code{Q}: glacier discharge \eqn{[mm/\Delta t]}.
//'   \item \code{SG}: glacier's bucket water storage content series \eqn{[1/\Delta t]}.
//' }
//'
//' @references
//' Jansson, P., Hock, R., Schneider, T., 2003. The concept of glacier storage: a review.
//' J. Hydrol., Mountain Hydrology and Water Resources 282, 116–129.
//' https://doi.org/10.1016/S0022-1694(03)00258-0
//'
//' Stahl, K., Moore, R.D., Shea, J.M., Hutchinson, D., Cannon, A.J., 2008. Coupled
//' modelling of glacier and streamflow response to future climate scenarios.
//' Water Resour. Res. 44, W02422. https://doi.org/10.1029/2007WR005956
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## Create an input data and run the module
//' DataMatrix <- cbind(
//'                     runif(n = 100, min = 0, max = 50),
//'                     runif(n = 100, min = 0, max = 200)
//'                     )
//'
//' dischGl    <- Glacier_Disch(model = 1, inputData = DataMatrix,
//'                            initCond = 100, param = c(0.1, 0.9, 10))
//'
//' @export
//'
//'
// [[Rcpp::export]]
NumericMatrix Glacier_Disch(int model,
                            NumericMatrix inputData,
                            double initCond,
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

  // param
  int chk_3 = sum( is_na(param) );
  if(chk_3 != 0){

    stop("param argument should not contain NA values!");

  }

  // *********************
  //  models
  // *********************
  if (model == 1) {
    //MODELO Stahl et al. 2008

    // Reviso datos de entrada
    if (inputData.ncol() < 2) {
      stop("Please verify inputData matrix");
    }
    if (param.size() < 3) {
      stop("Please verify param vector");
    }

    int n = inputData.nrow(); // número de filas
    int m = 2;                // número de columnas
    NumericMatrix out(n, m);

    double KGmin, dKG, AG;
    double Q, KG, SG;

    // Asigno parámetros
    KGmin = param[0];
    dKG   = param[1];
    AG    = param[2];

    for (int i = 0; i < n; ++i) {
      KG = std::min( KGmin + dKG * exp(-inputData(i, 0) / AG), 1.0);
      if (i == 0){
        SG = inputData(i, 1) + initCond;
      } else {
        SG = std::max( (inputData(i, 1) - Q) + SG, 0.0);
      }
      Q = KG * SG;

      // Relleno matriz de salida
      out(i, 0) = Q;
      out(i, 1) = SG;
    }

    colnames(out) = CharacterVector::create("Q", "SG");
    return out;

  } else {
    stop("Model not available");

  }

}
