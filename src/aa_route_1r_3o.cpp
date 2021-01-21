#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix route_1r_3o(NumericMatrix inputData,
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
    stop("In model 5, the initCond should be a vector of lenght one: SLZ0.");
  }

  // param
  int k3 = param.length();
  if(k3 != 5){
    stop("In model 5, the param vector argument should contain the following parameters: K0, K1, K2, UZL and PERC.");
  }


  // *********************
  //  function
  // *********************

  // CASO 5: UN RESERVORIO CON TRES SALIDAS //
  int n = inputData.nrow(); //número de filas de matriz de salida
  int m = 5;                //número de columnas de matriz de salida
  NumericMatrix out(n, m);

  // Defino variables
  double K0, K1, K2, UZL, PERC;  //parámetros
  double SLZ;                    //almacenamientos
  double Qg, Q2, Q1, Q0;         //caudales

  //Asigno valores para trabajar más cómodo y verifico condiciones
  K0   = param[0];
  K1   = param[1];
  K2   = param[2];
  UZL  = param[3];
  PERC = param[4];

  if ( (1.0 <= K0) | (K0 <= K1) | (K1 <= K2) | (UZL <= PERC) ) {
    stop("Please verify: 1 > K0 > K1 > K2 & UZL > PERC");
  } else {
    for (int i = 0; i < n; ++i) {
      // Condiciones iniciales
      if (i == 0) {
        SLZ = initCond[0];
      }

      // Reservorio
      if (SLZ > UZL) {
        Q0  = (SLZ - UZL + inputData(i, 0)) * K0;
        SLZ = (1 / K0 - 1) * Q0 + UZL;

        Q1  = (SLZ - PERC) * K1;
        SLZ = (1 / K1 - 1) * Q1 + PERC;

        Q2  = SLZ * K2;
        SLZ = SLZ - Q2;

      } else if (SLZ > PERC) {
        Q0  = 0.0;

        Q1  = (SLZ - PERC + inputData(i, 0)) * K1;
        SLZ = (1 / K1 - 1) * Q1 + PERC;

        Q2  = SLZ * K2;
        SLZ = SLZ - Q2;

      } else {
        Q0  = 0.0;

        Q1  = 0.0;

        Q2  = (SLZ + inputData(i, 0)) * K2;
        SLZ = (1 / K2 - 1) * Q2;
      }

      Qg = Q2 + Q1 + Q0;

      // Asigno salidas
      out(i, 0) = Qg;
      out(i, 1) = Q0;
      out(i, 2) = Q1;
      out(i, 3) = Q2;
      out(i, 4) = SLZ;

    } // cierre loop for()

  }

  colnames(out) = CharacterVector::create("Qg", "Q0", "Q1", "Q2", "SLZ");
  return out;


}

