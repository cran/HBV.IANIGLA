#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELOS DE EVAPOTRANSPIRACIÓN
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3
// Institución : IANIGLA-CONICET
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

// ELECCIÓN DEL MODELO QUE VOY A CORRER - model
// #1# Sinusoidal -> Calder et al. (1983)

// HEMISFERIO EN EL QUE CORRO EL MODELO - hemis *
// #1# Hemisferio sur
// #2# Hemisferio norte
// *NOTA: sólo aplicable al modelo sinusoidal. En el resto de los modelos se debe proporcionar
// un valor pero el mismo no tiene efecto.

// DATOS DE ENTRADA - inputData
// Modelo sinusoidal
// #1# Julian dates: serie de días julianos para los cuales se debe calcular PET

// ELEVACIÓN - elev
// Modelo sinusoidal
// #1# zref: altura de referencia en la cuál conozco el valor aproximado de evapotranspiración
  // #2# ztopo: altura en la cuál me interesa calcular la PET

//PARÁMTEROS A CALIBRAR - param
// Modelo sinusoidal
// #1# PET: evapotranspiración potencial media diaria climatológica [mm]
// #2# gradPET: gradiente de disminución de evapotranspiración con altura [mm/100 m]

// SALIDA
// Modelo 1: Sinusoidal - Calder et al. (1983)
// Serie de evapotranspiración potencial diaria


//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!


// [[Rcpp::export]]
NumericVector PET(int model, int hemis, NumericMatrix inputData, NumericVector elev, NumericVector param) {
  if (model == 1) {
    // SINUSOIDAL - Calder et al. (1983)

    if (hemis == 1) {
      // Hemisferio Sur
      int n = inputData.size();

      // Declaro variables
      double zref, ztopo;
      double EP, gradPET;
      NumericVector out(n);

      // Asigno valores a variables
      zref  = elev[0];
      ztopo = elev[1];

      EP      = param[0];
      gradPET = param[1];

      // Corro modelo
      for (int i = 0; i < n; ++i) {
        if (inputData(i, 0) == 0.0) { // PET igual a cero
          out[i] = 0.0;

        } else {

          out[i] = std::max(EP * ( 1 + std::sin( (360 * inputData[i] / 366 + 90) * M_PI / 180 ) ) +
            (ztopo -  zref) * (gradPET / 100), 0.0);

        }

      }

      return out;


    } else if (hemis == 2) {
      // Hemisferio Norte
      int n = inputData.size();

      // Declaro variables
      double zref, ztopo;
      double EP, gradPET;
      NumericVector out(n);

      // Asigno valores a variables
      zref  = elev[0];
      ztopo = elev[1];

      EP      = param[0];
      gradPET = param[1];

      // Corro modelo
      for (int i = 0; i < n; ++i) {
        if (inputData(i, 0) == 0.0) { // PET igual a cero
          out[i] = 0.0;

        } else {

          out[i] = std::max(EP * ( 1 + std::sin( (360 * inputData[i] / 366 - 90) * M_PI / 180 ) ) +
            (ztopo -  zref) * (gradPET / 100), 0.0);

        }

      }

      return out;

    } else {

      stop("Hemisphere must be 1 or 2");

    }

  } else {

    stop("Model not available");

  }
} // cierre función
