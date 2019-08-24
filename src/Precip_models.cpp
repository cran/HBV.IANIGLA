#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELOS PARA OBTENER SERIES DE  PRECIPITACIÓN
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3 
// Institución : IANIGLA-CONICET 
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

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

// [[Rcpp::export]]
NumericVector Precip_model(int model, NumericVector inputData, double zmeteo,
                          double ztopo, NumericVector param) {
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
