#include <Rcpp.h>
#include "median.h"
using namespace Rcpp;

//#########################################################################
// MODELOS DE HIDROGRAMAS UNITARIOS
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3
// Institución : IANIGLA-CONICET
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

// MODELO DE HIDROGRAMA UNITARIO - model
// #1# Hidrograma unitario triangular estático

// DATOS DE ENTRADA - inputData
// #1# Qg: salida total de los reservorios

// PARÁMETROS - param
// Modelo 1: HU triangular estático
// #1# Bmax: máximo retraso. Base del HU triangular

// SALIDA - output
// Modelo 1: HU triangular estático
// #1# Q: hidrograma de salida de la cuenca/subcuenca

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!

// [[Rcpp::export]]
NumericVector UH(int model, NumericVector Qg, NumericVector param){
  if (model == 1){
    // HU TRIANGULAR ESTÁTICO //

  // Le doy nombre a parámetro
  double Bmax = param[0];

  // Verifico que Bmax sea mayor o igual a 1
  if (Bmax < 1){
    stop("Parameter must be Bmax >= 1");
  }
  // Defino tamaño de vectores
  double n = ceil(Bmax); // intervalos en que divido el HU
  int m = Qg.size();  // tamaño del vector de salida
  int j;              // contador que uso para un loop

  // Defino variables y salida
  double hm, eps, Tp;  // HU de base uno
  NumericVector t(n), h(n), w(n);
  NumericVector out(m);

  // Doy valores
  hm  = 2.0;
  eps = 4.0;
  Tp  = 0.5;


  if (n == 1) {
    out = Qg;
    return out;

  } else {
    t[0] = 1 / Bmax;
    h[0] = hm - std::abs(t[0] - Tp) * eps;

    for (int i = 1; i < (n - 1); ++i) {
      t[i] = t[i - 1] + 1 / Bmax;
      h[i] = hm - std::abs(t[i] - Tp) * eps;
    }

    t[n - 1] = 1.0;
    h[n - 1] = 0.0;

    // Cálculo de ponderadores
    if (Bmax < 2) {
      w[0] = 0.5 + (hm + h[0]) * (t[0] - Tp) * 0.5;
      w[1] = 1 - w[0];

    } else {
      w[0] = t[0] * h[0] * 0.5;
      j    = medianCpp(n) - 1;
      for (int i = 1; i < n; ++i) {
       if (i == j) {
         w[i] = (h[i - 1] + hm) * (Tp - t[i -1]) * 0.5 + (h[i] + hm) * (t[i] - Tp) * 0.5;

       } else {
         w[i] = (h[i] + h[i - 1]) * (t[i] - t[i - 1]) * 0.5;

       }
      } // cierre loop for()

    }

    // Cálculo del hidrograma de salida
    for (int i = 0; i < m; ++i) {
      double Qf = 0.0; // variable ficticia

      for (int j = 0; j < n; ++j) {
        Qf += Qg[i - j] * w[j];

      }

      out[i] = Qf;



      }


    return out;
  }

  } else {
    stop("Model not available");
  }
}
