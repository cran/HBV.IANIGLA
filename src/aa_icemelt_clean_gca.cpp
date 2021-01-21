#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix icemelt_clean_gca(NumericMatrix inputData,
                                NumericVector initCond,
                                NumericVector param){
  // Genero el vector de salida
  int n = inputData.nrow(); // número filas
  int m = 9;                // número de columnas
  NumericMatrix out(n, m);

  // Declaro variables que voy a utilizar y nombre de parámetros
  double Prain, Psnow, SWE, Msnow, Mice, Mtot, Cum, Total, TotScal;
  double SFCF, Tt, Tm, fm, fi;

  // Asigno valores a los parámetros
  SFCF = param[0];
  Tt   = param[1];
  Tm   = param[2];
  fm   = param[3];
  fi   = param[4];

  // Corro rutina nivo-glaciar
  for (int i = 0; i < n; ++i){

    // Precipitación líquida o sólida
    if (inputData(i, 0) > Tt){
      Prain = inputData(i, 1);
      Psnow = 0.0;
    } else {
      Prain = 0.0;
      Psnow = inputData(i, 1) * SFCF;
    }

    // Nieve y hielo derretidos

    if (i == 0) {SWE = initCond[0];} //me aeguro el condicional para la primer corrida
    if (inputData(i, 0) > Tm) {//temperatura del aire MAYOR a Tmelt

      if (SWE == 0.0) { //no hay nieve

        Msnow   = 0.0;
        Mice    = (inputData(i, 0) - Tm) * fi;
        Mtot    = Msnow + Mice;
        SWE    += Psnow - Msnow;
        Cum     = Psnow - Mtot;
        Total   = (Mtot + Prain);
        TotScal = (Mtot + Prain) * inputData(i, 2);

      } else { //hay nieve

        Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
        Mice    = 0.0;
        Mtot    = Msnow + Mice;
        SWE    += Psnow - Msnow;
        Cum     = Psnow - Mtot;
        Total   = (Mtot + Prain);
        TotScal = (Mtot + Prain) * inputData(i, 2);
      }
    } else { //temperatura del aire MENOR a Tmelt

      Msnow   = 0.0;
      Mice    = 0.0;
      Mtot    = Msnow + Mice;
      SWE    += Psnow - Msnow;
      Cum     = Psnow - Mtot;
      Total   = (Mtot + Prain);
      TotScal = (Mtot + Prain) * inputData(i, 2);
    }

    // Relleno matriz de salida
    out(i, 0) = Prain;
    out(i, 1) = Psnow;
    out(i, 2) = SWE;
    out(i, 3) = Msnow;
    out(i, 4) = Mice;
    out(i, 5) = Mtot;
    out(i, 6) = Cum;
    out(i, 7) = Total;
    out(i, 8) = TotScal;
  }

  colnames(out) = CharacterVector::create("Prain", "Psnow", "SWE", "Msnow", "Mice", "Mtot", "Cum", "Total", "TotScal");
  return out;

}
