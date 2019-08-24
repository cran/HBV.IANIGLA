#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELOS PARA OBTENER SERIES DE TEMPERATURA
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3 
// Institución : IANIGLA-CONICET 
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

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

// [[Rcpp::export]]
NumericVector Temp_model(int model, NumericVector inputData, double zmeteo,
                          double ztopo, NumericVector param) {
  if (model == 1){ 
    // MODELO CON GRADIENTE LINEAL //
    
    int n = inputData.size();
    NumericVector out(n);
    double gradT = param[0];
    
    for (int i = 0; i < n; ++i) {
      //serie de temperaturas
      out[i] = (ztopo - zmeteo) * (gradT / 100) + inputData[i];
      
    }
    
    return out;
    
  } else {
    stop("Model not available");
    
  }
  
}
