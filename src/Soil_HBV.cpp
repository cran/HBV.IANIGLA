#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELO DE SUELO DEL HBV
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3
// Institución : IANIGLA-CONICET
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

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


// [[Rcpp::export]]
NumericVector Soil_HBV(int model, NumericMatrix inputData, NumericVector initCond, NumericVector param) {

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
    if ( (param[1] > 1) | (param[1] <= 0) ) {
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
    if ( (param[1] > 1) | (param[1] <= 0) ) {
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
