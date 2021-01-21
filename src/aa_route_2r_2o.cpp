#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix route_2r_2o(bool lake,
                          NumericMatrix inputData,
                          NumericVector initCond,
                          NumericVector param) {
  // *********************
  //  conditionals
  // *********************
  // lake
  if( (lake != false) & (lake != true) ){

    stop("In model 2, lake option must be either TRUE or FALSE");

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
  if(k2 != 2){
    stop("In model 2, the initCond should be a vector of lenght two: SLZ0 and SUZ0.");
  }

  // param
  int k3 = param.length();
  if(k3 != 3){
    stop("In model 2, the param vector argument should contain the following parameters: K1, K2 and PERC");
  }


  // *********************
  //  function
  // *********************

  // CASO 2: DOS RESERVORIOS EN SERIE //
  int n = inputData.nrow(); //número de filas de matriz de salida
  int m = 5;                //número de columnas de matriz de salida
  NumericMatrix out(n, m);

  // Defino variables
  double K1, K2, PERC; //parámetros
  double UpLow;        //variables intermedias
  double SUZ, SLZ;     //almacenamientos
  double Qg, Q2, Q1;        //caudales

  //Asigno valores para trabajar más cómodo y verifico condiciones
  K1   = param[0];
  K2   = param[1];
  PERC = param[2];

  if ( (1.0 <= K1) | (K1 <= K2) ) {
    stop("Please verify: 1 > K1 > K2");
  } else {
    for (int i = 0; i < n; ++i){

      if (i == 0) {
        // Condición de inicio
        SLZ = initCond[0];
        SUZ = initCond[1];
      }

      // Primer reservorio
      if (SUZ >= PERC) {
        UpLow = PERC;
        Q1    = (SUZ + inputData(i, 0) - UpLow) * K1;
        SUZ   = (1 / K1 - 1) * Q1;

      } else {
        UpLow = SUZ;
        Q1    = 0.0;
        SUZ   = inputData(i, 0);
      }

      // Segundo reservorio
      // sin lago
      if (lake == false) {
        Q2  = (SLZ + UpLow) * K2;
        SLZ = (1 / K2 - 1) * Q2;

        // con lago
      } else if (lake == true) {

        if (SLZ + inputData(i, 1) >= inputData(i, 2)) {
          Q2  = (SLZ + inputData(i, 1) - inputData(i, 2) + UpLow) * K2;
          SLZ = (1 / K2 - 1) * Q2;

        } else {
          Q2  = 0.0;
          SLZ = UpLow;

        }

      }

      Qg = Q2 + Q1;

      // Asigno salidas
      out(i, 0) = Qg;
      out(i, 1) = Q1;
      out(i, 2) = Q2;
      out(i, 3) = SUZ;
      out(i, 4) = SLZ;

    } //cierre loop for()
  }

  colnames(out) = CharacterVector::create("Qg", "Q1", "Q2", "SUZ", "SLZ");
  return(out);



}
