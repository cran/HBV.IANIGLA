#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELOS DE RESERVORIOS
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3
// Institución : IANIGLA-CONICET
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

// MODELO DE RESERVORIO - model
// Mirar las notas en papel (y en un futuro el manual) para una descripción gráfica
// Ingreso número entero según modelo elejido
// #1# Caso 1: tres reservorios en serie
// #2# Caso 2: dos reservorios en serie
// #3# Caso 3: dos reservorios con tres salidas
// #4# Caso 4: un reservorio con dos salidas   -> PROBAR EN ALUVIONES (FLASH FLOODS) O CUENCAS PEQUEÑAS
// #5# Caso 5: un reservorio con tres salidas  -> PROBAR EN ALUVIONES (FLASH FLOODS) O CUENCAS PEQUEÑAS

// NOTA: solo en los modelos 1 a 3 se pueden incluir lagos

// LAGOS - lake
// valor lógico que me indica si la cuenca tiene o cuerpos lacustres
// # TRUE
// # FALSE

// DATOS DE ENTRADA - inputData
// Matriz con recarga total de la cuenca/subcuenca, precipitación y evaporación en lago
// #1# Ieff  : serie de entrada efectivo [mm/deltaT]
// #2# precip: serie de precipitación ESCALADA (relatArea) en el lago [mm/deltaT]    -> OPCIONAL
// #3# evap  : serie de evaporación ESCALADA (relatArea) en el lago [mm/deltaT]      -> OPCIONAL

// CONDICIONES INICIALES - initCond
// Vector con los siguientes datos
// #1# SLZ0: contenido de agua inicial en el reservorio inferior [mm]
// #2# SUZ0: contenido de agua inicial en el reservorio medio [mm]    -> OPCIONAL - DEPENDE DEL MODELO
// #3# STZ0: contenido de agua inicial en el reservorio superior [mm] -> OPCIONAL - DEPENDE DEL MODELO

// PARÁMTEROS A CALIBRAR - param
// Vector de parámetros
// En general se debe cumplir con:
//  1 > K0 > K1 > K2
//  UZL > PERC

// Caso 1: tres reservorios en serie
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : tasa máxima de flujo de agua entre STZ y SUZ [mm/deltaT]
// #5# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 2: dos reservorios en serie
// #1# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #2# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #3# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 3: dos reservorios con tres salidas
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : contenido mínimo de agua que debe pasar SUZ para que ocurra Q0 [mm]
// #5# PERC: tasa máxima de flujo de agua entre SUZ y SLZ [mm/deltaT]

// Caso 4: un reservorio con dos salidas
// #1# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #2# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #3# PERC: contenido mínimo de agua que debe pasar SLZ para que ocurra Q1 [mm]

// Caso 5: un reservorio con tres salidas
// #1# K0  : inversa del tpo de tránsito en STZ  [1/deltaT]
// #2# K1  : inversa del tpo de tránsito en SUZ  [1/deltaT]
// #3# K2  : inversa del tpo de tránsito en SLZ  [1/deltaT]
// #4# UZL : contenido mínimo de agua que debe pasar SUZ para que ocurra Q0 [mm]
// #5# PERC: contenido mínimo de agua que debe pasar SLZ para que ocurra Q1 [mm]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!

// [[Rcpp::export]]
NumericMatrix Routing_HBV(int model, bool lake, NumericMatrix inputData, NumericVector initCond, NumericVector param){
  // PRIMERO ELIJO EL MODELO A CORRER

  if (model == 1) {
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
          int k = inputData.ncol();
          if (k != 3) {
            stop("Verify if precipitation and evaporation data is supplied");
          }

          if (SLZ + inputData(i, 1) > inputData(i, 2) ) {
            Q2  = (SLZ + inputData(i, 1) - inputData(i, 2) + UpLow) * K2;
            SLZ = (1 / K2 - 1) * Q2;
          } else {
            Q2  = 0.0;
            SLZ = UpLow;
          }

        } else {
          stop("Please specify if lake = TRUE | FALSE");
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

  } else if (model == 2) {
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
           int k = inputData.ncol();
           if (k != 3) {
             stop("Verify if precipitation and evaporation data is supplied");
           }

           if (SLZ + inputData(i, 1) >= inputData(i, 2)) {
             Q2  = (SLZ + inputData(i, 1) - inputData(i, 2) + UpLow) * K2;
             SLZ = (1 / K2 - 1) * Q2;

           } else {
             Q2  = 0.0;
             SLZ = UpLow;

           }

         } else {
           stop("Please specify if lake = TRUE | FALSE");
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

  } else if (model == 3) {
    // CASO 3: DOS RESERVORIOS EN SERIE CON TRES SALIDAS //
    int n = inputData.nrow(); //número de filas de matriz de salida
    int m = 6;                //número de columnas de matriz de salida
    NumericMatrix out(n, m);

    // Defino variables
    double K0, K1, K2, UZL, PERC; //parámetros
    double UpLow;                 //variables intermedias
    double SUZ, SLZ;              //almacenamientos
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

        // Condición de inicio
        if (i == 0) {
          SLZ = initCond[0];
          SUZ = initCond[1];

        }

        // Primer reservorio
        if (SUZ > UZL){
          Q0    = (SUZ - UZL + inputData(i, 0)) * K0;
          SUZ   = (1 / K0 - 1) * Q0 + UZL;

          if (SUZ >= PERC) {
            UpLow = PERC;
            Q1    = (SUZ - UpLow) * K1;
            SUZ   = (1 / K1 - 1) * Q1;

          } else {
            UpLow = SUZ;
            Q1    = 0.0;
            SUZ   = 0.0;
          }


        } else {
          Q0    = 0.0;

          if (SUZ >= PERC) {
            UpLow = PERC;
            Q1    = (SUZ + inputData(i, 0) - UpLow) * K1;
            SUZ   = (1 / K1 - 1) * Q1;

          } else {
            UpLow = SUZ;
            Q1    = 0.0;
            SUZ   = inputData(i, 0);
          }

        }

        // Segundo reservorio
        // sin lago
        if (lake == false) {
          Q2  = (SLZ + UpLow) * K2;
          SLZ = (1 / K2 - 1) * Q2;

          // con lago
        } else if (lake == true) {
          int k = inputData.ncol();
          if (k != 3) {
            stop("Verify if precipitation and evaporation data is supplied");
          }

          if (SLZ + inputData(i, 1) > inputData(i, 2) ) {
            Q2  = (SLZ + inputData(i, 1) - inputData(i, 2) + UpLow) * K2;
            SLZ = (1 / K2 - 1) * Q2;
          } else {
            Q2  = 0.0;
            SLZ = UpLow;
          }

        } else {
          stop("Please specify if lake = TRUE | FALSE");
        }

        Qg = Q2 + Q1 + Q0;

        // Asigno salidas
        out(i, 0) = Qg;
        out(i, 1) = Q0;
        out(i, 2) = Q1;
        out(i, 3) = Q2;
        out(i, 4) = SUZ;
        out(i, 5) = SLZ;


      } // cierre loop for()


  }

    colnames(out) = CharacterVector::create("Qg", "Q0", "Q1", "Q2", "SUZ", "SLZ");
    return out;

  } else if (model == 4) {
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



  } else if (model == 5) {
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

  } else {
    stop("Model not available");
  }

}//cierre función

