#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELOS PARA TRANSITAR CAUDAL A TRAVÉS DE GLACIARES
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3 
// Institución : IANIGLA-CONICET 
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

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

// [[Rcpp::export]]
NumericMatrix Glacier_Disch(int model, NumericMatrix inputData, double initCond, NumericVector param){
  
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
