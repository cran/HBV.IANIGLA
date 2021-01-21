#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix route_3r_3o(bool lake,
                          NumericMatrix inputData,
                          NumericVector initCond,
                          NumericVector param) {
  // *********************
  //  conditionals
  // *********************
  // lake
  if( (lake != false) & (lake != true) ){

    stop("In model 1, lake option must be either TRUE or FALSE");

  }

  // inputData
  int k1 = inputData.ncol();
  if(lake == true){
    if (k1 != 3) {
      stop("inputData argument should have three columns: effective runoff, lake precipitation and lake evaporation!");
    }

  } else{

    if(k1 != 1){
      stop("inputData argument should have one column: effective runoff!");
    }

  }


  // initCond
  int k2 = initCond.length();
  if(k2 != 3){
    stop("In model 1, the initCond should be a vector of lenght three: SLZ0, SUZ0, STZ0");
  }

  // param
  int k3 = param.length();
  if(k3 != 5){
    stop("In model 1, the param vector argument should contain the following parameters: K0, K1, K2, UZL and PERC");
  }






  // *********************
  //  function
  // *********************

  // CASO 1: TRES RESERVORIOS EN SERIE //
  int n = inputData.nrow(); //número de filas de matriz de salida
  int m = 7;                //número de columnas de matriz de salida
  NumericMatrix out(n, m);

  // Defino variables
  double K0, K1, K2, UZL, PERC; //parámetros
  double TopUp, UpLow;          //variables intermedias
  double STZ, SUZ, SLZ;         //almacenamientos
  double Qg, Q2, Q1, Q0;        //caudales

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

      if (i == 0) {
        // Condición de inicio
        SLZ = initCond[0];
        SUZ = initCond[1];
        STZ = initCond[2];
      }

      // Primer reservorio
      if (STZ >= UZL) {
        TopUp = UZL;
        Q0    = (STZ + inputData(i, 0) - TopUp) * K0;
        STZ   = (1 / K0 - 1) * Q0;

      } else {
        TopUp = STZ;
        Q0    = 0.0;
        STZ   = inputData(i, 0);
      }

      // Segundo reservorio
      if (SUZ >= PERC) {
        UpLow = PERC;
        Q1    = (SUZ + TopUp - UpLow) * K1;
        SUZ   = (1 / K1 - 1) * Q1;

      } else {
        UpLow = SUZ;
        Q1    = 0.0;
        SUZ   = TopUp;
      }

      //Tercer reservorio
      // sin lago
      if (lake == false) {
        Q2  = (SLZ + UpLow) * K2;
        SLZ = (1 / K2 - 1) * Q2;

        // con lago
      } else if (lake == true) {

        if (SLZ + inputData(i, 1) > inputData(i, 2) ) {
          Q2  = (SLZ + inputData(i, 1) - inputData(i, 2) + UpLow) * K2;
          SLZ = (1 / K2 - 1) * Q2;
        } else {
          Q2  = 0.0;
          SLZ = UpLow;
        }

      }

      Qg = Q2 + Q1 + Q0;

      // Asigno salidas
      out(i, 0) = Qg;
      out(i, 1) = Q0;
      out(i, 2) = Q1;
      out(i, 3) = Q2;
      out(i, 4) = STZ;
      out(i, 5) = SUZ;
      out(i, 6) = SLZ;

    } //cierre loop for()

  }

  colnames(out) = CharacterVector::create("Qg", "Q0", "Q1", "Q2", "STZ", "SUZ", "SLZ");
  return out;


}
