#include <Rcpp.h>
#include "aa_route_3r_3o.h"
#include "aa_route_2r_2o.h"
#include "aa_route_2r_3o.h"
#include "aa_route_1r_2o.h"
#include "aa_route_1r_3o.h"
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
// MODELO DE RESERVORIO - model
// Mirar las notas en papel (y en un futuro el manual) para una descripción gráfica
// Ingreso número entero según modelo elejido
// #1# Caso 1: tres reservorios en serie
// #2# Caso 2: dos reservorios en serie
// #3# Caso 3: dos reservorios con tres salidas
// #4# Caso 4: un reservorio con dos salidas   -> PROBAR EN ALUVIONES (FLASH FLOODS) O CUENCAS PEQUEÑAS
// #5# Caso 5: un reservorio con tres salidas  -> PROBAR EN ALUVIONES (FLASH FLOODS) O CUENCAS PEQUEÑAS

// NOTA: solo en los modelos 1 a 3 se pueden incluir lagos

// LAGOS - lake
// valor lógico que me indica si la cuenca tiene o cuerpos lacustres
// # TRUE
// # FALSE

// DATOS DE ENTRADA - inputData
// Matriz con recarga total de la cuenca/subcuenca, precipitación y evaporación en lago
// #1# Ieff  : serie de entrada efectivo [mm/deltaT]
// #2# precip: serie de precipitación ESCALADA (relatArea) en el lago [mm/deltaT]    -> OPCIONAL
// #3# evap  : serie de evaporación ESCALADA (relatArea) en el lago [mm/deltaT]      -> OPCIONAL

// CONDICIONES INICIALES - initCond
// Vector con los siguientes datos
// #1# SLZ0: contenido de agua inicial en el reservorio inferior [mm]
// #2# SUZ0: contenido de agua inicial en el reservorio medio [mm]    -> OPCIONAL - DEPENDE DEL MODELO
// #3# STZ0: contenido de agua inicial en el reservorio superior [mm] -> OPCIONAL - DEPENDE DEL MODELO

// PARÁMTEROS A CALIBRAR - param
// Vector de parámetros
// En general se debe cumplir con:
//  1 > K0 > K1 > K2
//  UZL > PERC

// Caso 1: tres reservorios en serie
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : tasa máxima de flujo de agua entre STZ y SUZ [mm/deltaT]
// #5# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 2: dos reservorios en serie
// #1# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #2# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #3# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 3: dos reservorios con tres salidas
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : contenido mínimo de agua que debe pasar SUZ para que ocurra Q0 [mm]
// #5# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 4: un reservorio con dos salidas
// #1# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #2# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #3# PERC: contenido mínimo de agua que debe pasar SLZ para que ocurra Q1 [mm]

// Caso 5: un reservorio con tres salidas
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : contenido mínimo de agua que debe pasar SUZ para que ocurra Q0 [mm]
// #5# PERC: contenido mínimo de agua que debe pasar SLZ para que ocurra Q1 [mm]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!

 */

//' @name Routing_HBV
//'
//' @title Routing bucket type models
//'
//' @description Implement one of the five different bucket formulations for
//' runoff routing. The output of this function is the input series of the
//' transfer function (\code{\link{UH}}).
//'
//' @usage Routing_HBV(
//'        model,
//'        lake,
//'        inputData,
//'        initCond,
//'        param
//'        )
//'
//' @param model numeric integer indicating which reservoir formulation to use:
//' \itemize{
//'   \item 1: Three series of reservoirs. Lake option is allowed.
//'
//'   \if{html}{\figure{bucket_3_outlet_3.png}{options: width=200}}
//'
//'   \item 2: Two series of reservoirs. Lake option is allowed.
//'
//'   \if{html}{\figure{bucket_2_outlet_2.png}{options: width=200}}
//'
//'   \item 3: Two reservoirs and three outlets. Lake option is allowed.
//'
//'   \if{html}{\figure{bucket_2_outlet_3.png}{options: width=200}}
//'
//'   \item 4: One reservoir and two outlets. Lake is NOT allowed.
//'
//'   \if{html}{\figure{bucket_1_outlet_2.png}{options: width=200}}
//'
//'   \item 5: One reservoir and three outlets. Lake is NOT allowed.
//'
//'   \if{html}{\figure{bucket_1_outlet_3.png}{options: width=200}}
//'
//'}
//'
//' @param lake logical. A \code{TRUE} value will enable the lake option (only available
//' on \strong{models 1, 2 and 3}). When modeling a lake, HBV.IANIGLA considers
//' that this water body exist on the bottom bucket, so you will also have to provide
//' a lake evaporation and precipitation series in the \strong{\code{inputData}} matrix.
//'
//' @param inputData numeric matrix with three columns (two of them depends on
//' \strong{\code{lake}} option).
//' \itemize{
//'   \item \code{column_1}: effective runoff series \eqn{[mm/\Delta T]}. This is
//'   the output of the \code{\link{Soil_HBV}} module.
//'   \item \code{column_2}: only if  \strong{\code{lake = TRUE}}. Precipitation series
//'   falling in the lake. \strong{When using it remember that the precipitation should
//'   be rescaled according to the relative area of the lake in the basin}.
//'   \item \code{column_3}: only if  \strong{\code{lake = TRUE}}. Lake's evaporation
//'   series. \strong{When using it remember that the precipitation should
//'   be rescaled according to the relative area of the lake in the basin}.
//' }
//'
//' @param initCond numeric vector with the following initial state variables.
//' \itemize{
//'   \item \code{SLZ0}: initial water content of the lower reservoir \eqn{[mm]}. This
//'   state variable is compulsory for all \strong{model} options.
//'   \item \code{SUZ0}: initial water content of the intermediate reservoir \eqn{[mm]}.
//'   This option does not make sense for \strong{models 4 and 5}.
//'   \item \code{STZ0}: initial water content of the upper reservoir \eqn{[mm]}.
//'   This option only make sense for \strong{model 1}.
//'
//' }
//'
//' @param param numeric vector. The length depends on the \strong{model}'s choice:
//'
//' \strong{Model 1}
//' \itemize{
//'   \item \code{K0}: top bucket (\code{STZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{UZL}: maximum flux rate between \code{STZ} and
//'   \code{SUZ} \eqn{[mm/\Delta t]}.
//'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
//'   \code{SLZ} \eqn{[mm/\Delta t]}.
//'}
//'
//' \strong{Model 2}
//' \itemize{
//'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
//'   \code{SLZ} \eqn{[mm/\Delta t]}.
//'}
//'
//' \strong{Model 3}
//' \itemize{
//'   \item \code{K0}: top output (\code{Q0}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K1}: intermediate bucket (\code{SUZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{UZL}: minimum water content of \code{SUZ} for supplying fast runoff
//'   (\code{Q0}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
//'   \item \code{PERC}: maximum flux rate between \code{SUZ} and
//'   \code{SLZ} \eqn{[mm/\Delta t]}.
//'}
//'
//' \strong{Model 4}
//' \itemize{
//'   \item \code{K1}: intermediate output (\code{Q1}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{PERC}: minimum water content of \code{SLZ} for supplying intermediate
//'   runoff (\code{Q1}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
//' }
//'
//' \strong{Model 5}
//' \itemize{
//'   \item \code{K0}: top output (\code{Q0}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K1}: intermediate output (\code{Q1}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{K2}: lower bucket (\code{SLZ}) storage constant \eqn{[1/\Delta t]}.
//'   \item \code{UZL}: minimum water content of \code{SLZ} for supplying fast runoff
//'   (\code{Q0}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
//'   \item \code{PERC}: minimum water content of \code{SLZ} for supplying intermediate
//'   runoff (\code{Q1}) to the total reservoir discharge (\code{Qg}) \eqn{[mm]}.
//'}
//'
//' @return Numeric matrix with the following columns:
//'
//' \strong{Model 1}
//' \itemize{
//'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q0}: top bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{STZ}: top reservoir storage \eqn{[mm]}.
//'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
//'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
//'}
//'
//'\strong{Model 2}
//' \itemize{
//'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
//'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
//'}
//'
//' \strong{Model 3}
//' \itemize{
//'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q0}: intermediate bucket fast discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q1}: intermediate bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{SUZ}: intermediate reservoir storage \eqn{[mm]}.
//'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
//'}
//'
//' \strong{Model 4}
//' \itemize{
//'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q1}: lower bucket intermediate discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
//'}
//'
//' \strong{Model 5}
//' \itemize{
//'   \item \code{Qg}: total buckets output discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q0}: lower bucket fast discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q1}: lower bucket intermediate discharge \eqn{[mm/\Delta t]}.
//'   \item \code{Q2}: lower bucket discharge \eqn{[mm/\Delta t]}.
//'   \item \code{SLZ}: lower reservoir storage \eqn{[mm]}.
//'}
//'
//' @examples
//' # The following is a toy example. I strongly recommend to see
//' # the package vignettes in order to improve your skills on HBV.IANIGLA
//'
//' ## Case example with the first model
//' inputMatrix <- cbind(
//'                      runif(n = 200, max = 100, min = 0),
//'                      runif(n = 200, max = 50, min = 5),
//'                      runif(n = 100, max = 3, min = 1)
//'                      )
//'
//' routeMod1   <- Routing_HBV(model = 1, lake = TRUE, inputData = inputMatrix,
//'                      initCond = c(10, 15, 20), param = c(0.1, 0.05, 0.001, 1, 0.8))
//'
//'
//' @references
//' Bergström, S., Lindström, G., 2015. Interpretation of runoff processes in
//' hydrological modelling—experience from the HBV approach. Hydrol. Process. 29, 3535–3545.
//' https://doi.org/10.1002/hyp.10510
//'
//' Beven, K.J., 2012. Rainfall - Runoff Modelling, 2 edition. ed. Wiley, Chichester.
//'
//' Seibert, J., Vis, M.J.P., 2012. Teaching hydrological modeling with a user-friendly
//' catchment-runoff-model software package. Hydrol Earth Syst Sci 16, 3315–3325.
//' https://doi.org/10.5194/hess-16-3315-2012
//'
//'
//' @export
//'
// [[Rcpp::export]]
NumericMatrix Routing_HBV(int model,
                          bool lake,
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

  // PRIMERO ELIJO EL MODELO A CORRER

  if (model == 1) {
    NumericMatrix out = route_3r_3o(lake,
                                    inputData,
                                    initCond,
                                    param);
    return(out);

  } else if (model == 2) {

    NumericMatrix out = route_2r_2o(lake,
                                    inputData,
                                    initCond,
                                    param);

    return(out);

  } else if (model == 3) {

    NumericMatrix out = route_2r_3o(lake,
                                    inputData,
                                    initCond,
                                    param);

    return(out);

  } else if (model == 4) {

    NumericMatrix out = route_1r_2o(inputData,
                                    initCond,
                                    param);

    return(out);

  } else if (model == 5) {

    NumericMatrix out = route_1r_3o(inputData,
                                    initCond,
                                    param);

    return(out);

  } else {

    stop("Model not available");

  }

}//cierre función

