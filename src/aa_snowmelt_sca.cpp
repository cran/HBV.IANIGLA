#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix snowmelt_sca(NumericMatrix inputData,
                           NumericVector initCond,
                           NumericVector param){

  // Genero el vector de salida
  int n = inputData.nrow(); // número filas
  int m = 6;                // número de columnas
  NumericMatrix out(n, m);

  // Declaro variables que voy a utilizar y nombre de parámetros
  double Prain, Psnow, SWE, Msnow, Total, TotScal;
  double SCA;
  bool flag;
  double SFCF, Tt, Tm, fm;

  // Asigno valores a los parámetros
  SFCF = param[0];
  Tt   = param[1];
  Tm   = param[2];
  fm   = param[3];

  // Corro rutina nival
  for (int i = 0; i < n; ++i){

    // Verifico si SCA[i] == NA (true) o no (false)
    flag = NumericVector::is_na(inputData(i, 2));

    // Denfino el valor de SCA
    if (i == 0) {
      if (flag == true) {
        SCA = 1;
      } else {
        SCA = inputData(i, 2);
      }
    } else {
      if (flag == false) {
        SCA = inputData(i, 2);
      }
    }

    // Precipitación líquida o sólida
    if (inputData(i, 0) > Tt){
      Prain = inputData(i, 1);
      Psnow = 0.0;
    } else {
      Prain = 0.0;
      Psnow = inputData(i, 1) * SFCF;
    }

    // Nieve derretida
    if (i == 0) {SWE = initCond[0];} //me aeguro el condicional para la primer corrida
    if (inputData(i, 0) > Tm) {//temperatura del aire MAYOR a Tmelt

      if (SWE == 0.0) { //no hay nieve

        Msnow   = 0.0;
        SWE    += Psnow - Msnow;
        Total   = (Msnow + Prain);
        TotScal = (Msnow * SCA + Prain);

      } else { //hay nieve

        Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
        SWE  += Psnow - Msnow;
        Total = (Msnow + Prain);
        TotScal = (Msnow * SCA + Prain);
      }
    } else {//temperatura del aire MENOR a Tmelt

      Msnow = 0.0;
      SWE  += Psnow - Msnow;
      Total = (Msnow + Prain);
      TotScal = (Msnow * SCA + Prain);

    }

    // Relleno matriz de salida
    out(i, 0) = Prain;
    out(i, 1) = Psnow;
    out(i, 2) = SWE;
    out(i, 3) = Msnow;
    out(i, 4) = Total;
    out(i, 5) = TotScal;

  }

  colnames(out) = CharacterVector::create("Prain", "Psnow", "SWE", "Msnow", "Total", "TotScal");
  return out;

}
