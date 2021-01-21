#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix route_1r_2o(NumericMatrix inputData,
                          NumericVector initCond,
                          NumericVector param) {
  // *********************
  //  conditionals
  // *********************

  // inputData
  int k1 = inputData.ncol();
  if(k1 != 1){

      stop("inputData argument should have one column: effective runoff!");

    }

  // initCond
  int k2 = initCond.length();
  if(k2 != 1){
    stop("In model 4, the initCond should be a vector of lenght one: SLZ0.");
  }

  // param
  int k3 = param.length();
  if(k3 != 3){
    stop("In model 4, the param vector argument should contain the following parameters: K1, K2 and PERC.");
  }


  // *********************
  //  function
  // *********************

  // CASO 4: UN RESERVORIO CON DOS SALIDAS //
  int n = inputData.nrow(); //número de filas de matriz de salida
  int m = 4;                //número de columnas de matriz de salida
  NumericMatrix out(n, m);

  // Defino variables
  double K1, K2, PERC;  //parámetros
  double SLZ;           //almacenamientos
  double Qg, Q2, Q1;    //caudales

  //Asigno valores para trabajar más cómodo y verifico condiciones
  K1   = param[0];
  K2   = param[1];
  PERC = param[2];

  if ( (1.0 <= K1) | (K1 <= K2) ) {
    stop("Please verify: 1 > K1 > K2");
  } else {
    for (int i = 0; i < n; ++i) {
      // Condiciones iniciales
      if (i == 0) {
        SLZ = initCond[0];
      }

      //Reservorio
      if (SLZ > PERC) {
        Q1  = (SLZ - PERC + inputData(i, 0)) * K1;
        SLZ = (1 / K1 - 1) * Q1 + PERC;
        Q2  = SLZ * K2;
        SLZ = SLZ - Q2;

      } else {
        Q1  = 0.0;
        Q2  = (SLZ + inputData(i, 0)) * K2;
        SLZ = (1 / K2 - 1) * Q2;
      }

      Qg = Q2 + Q1;

      // Asigno salidas
      out(i, 0) = Qg;
      out(i, 1) = Q1;
      out(i, 2) = Q2;
      out(i, 3) = SLZ;

    } //cierre loop for()
  }

  colnames(out) = CharacterVector::create("Qg", "Q1", "Q2", "SLZ");
  return out;



}
