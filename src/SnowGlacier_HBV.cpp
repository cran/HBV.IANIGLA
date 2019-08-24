#include <Rcpp.h>
using namespace Rcpp;

//#########################################################################
// MODELO NIVOGLACIAR
//#########################################################################

//#########################################################################
// Autor       : Ezequiel Toum
// Licencia    : GPL V3 
// Institución : IANIGLA-CONICET 
// e-mail      : etoum@mendoza-conicet.gob.ar
//#########################################################################

// ELECCIÓN DEL MODELO QUE VOY A CORRER - model 
// #model:     (1) modelo TI ; (2) modelo TI con SCA ; (3) modelo TI con área glaciar variable

// DATOS DE ENTRADA - inputData
  // Modelo TI 
// #1# airT:     serie de temperatura máxima [°C/deltaT]
// #2# precip:   serie de precipitaciones [mm/deltaT]

  // Modelo TI con SCA
// #1# airT:     serie de temperatura máxima [°C/deltaT]
// #2# precip:   serie de precipitaciones [mm/deltaT]
// #3# SCA:      serie de cobertura nívea [0 ; 1] [-]

  // Modelo TI con área glaciar variable
// #1# airT:   serie de temperatura máxima [°C/deltaT]
// #2# precip: serie de precipitaciones [mm/deltaT]
// #3# GCA*:   serie de cobertura glaciar relativo a toda la cuenca [-]  

  // *GCA:  Glacier Covered Area

// CONDICIONES INICIALES - initCond
// #1# initCond:  valor inicial del equivalente agua nieve [mm]                          
// #2# initCond:  tipo de superficie debajo de la nieve (1)glaciar descubierto - (2)suelo - (3) glaciar cubierto
// #3# initCond:  area relativa de la zona o banda de elevación con respecto a la cuenca *
// *NOTA: sólo requerido cuando la superficie es glaciar en los modelos 1 y 2

//PARÁMTEROS A CALIBRAR - param
  // Modelos TI, TI con SCA y TI con serie GCA
// #1# SFCF: factor de corrección de caída de precipitación [-]
// #2# Tr  : temperatura que divide precipitación líquida de sólida [°C]
// #3# Tt  : temperatura umbral de derretimiento [°C]
// #4# fm  : factor de derretimiento para nieve [mm/°C.deltaT]
// #5# fi  : factor de derretimiento para hielo [mm/°C.deltaT]
// #6# fic : factor de derretimiento para hielo cubierto [mm/°C.deltaT]

//TENER EN CTA QUE LOS INDICES EMPIEZAN EN CERO!!!!!!!
//LOS CEROS PARA DOUBLES VAN COMO 0.0!!!!

// [[Rcpp::export]]
NumericMatrix SnowGlacier_HBV(int model, NumericMatrix inputData, NumericVector initCond,
                          NumericVector param){
  
  //#### MODELO ####//
  if (model == 1) {
    // MODELO TI
    
    // GLACIAR DESCUBIERTO, SUELO O GLACIAR CUBIERTO
    
    if (initCond[1] == 1) {
      // SUPERFICIE GLACIAR
      
      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 3) {
        stop("You must support the relative area of the glacier");
      } 
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }
      
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
          Prain = inputData(i, 1) * SFCF;
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
            TotScal = (Mtot + Prain) * initCond[2]; 
            
          } else { //hay nieve
            
            Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
            Mice    = 0.0;
            Mtot    = Msnow + Mice;
            SWE    += Psnow - Msnow;
            Cum     = Psnow - Mtot;
            Total   = (Mtot + Prain); 
            TotScal = (Mtot + Prain) * initCond[2]; 
          }
        } else { //temperatura del aire MENOR a Tmelt
          
          Msnow   = 0.0;
          Mice    = 0.0;
          Mtot    = Msnow + Mice;
          SWE    += Psnow - Msnow;
          Cum     = Psnow - Mtot;
          Total   = (Mtot + Prain); 
          TotScal = (Mtot + Prain) * initCond[2]; 
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
      
    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO
      
      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      } 
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }
      
      // Genero el vector de salida
      int n = inputData.nrow(); // número filas
      int m = 5;                // número de columnas
      NumericMatrix out(n, m);
      
      // Declaro variables que voy a utilizar y nombre de parámetros
      double Prain, Psnow, SWE, Msnow, Total;
      double SFCF, Tt, Tm, fm;
      
      // Asigno valores a los parámetros
      SFCF = param[0];
      Tt   = param[1];
      Tm   = param[2];
      fm   = param[3];
      
      // Corro rutina nival
      for (int i = 0; i < n; ++i){
        
        // Precipitación líquida o sólida
        if (inputData(i, 0) > Tt){
          Prain = inputData(i, 1) * SFCF; 
          Psnow = 0.0;
        } else {
          Prain = 0.0;
          Psnow = inputData(i, 1) * SFCF;
        }
        
        // Nieve derretida
        if (i == 0) {SWE = initCond[0];} //me aeguro el condicional para la primer corrida
        if (inputData(i, 0) > Tm) {//temperatura del aire MAYOR a Tmelt
          
          if (SWE == 0.0) { //no hay nieve
            
            Msnow = 0.0;
            SWE  += Psnow - Msnow;
            Total = (Msnow + Prain); 
            
          } else { //hay nieve
            
            Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
            SWE  += Psnow - Msnow;
            Total = (Msnow + Prain); 
          }
        } else {//temperatura del aire MENOR a Tmelt
          
          Msnow = 0.0;
          SWE  += Psnow - Msnow;
          Total = (Msnow + Prain); 
          
        }
        
        // Relleno matriz de salida
        out(i, 0) = Prain;
        out(i, 1) = Psnow;
        out(i, 2) = SWE;
        out(i, 3) = Msnow;
        out(i, 4) = Total;
        
      }
      
      colnames(out) = CharacterVector::create("Prain", "Psnow", "SWE", "Msnow", "Total");
      return out;
      
    } else if (initCond[1] == 3) {
      
     // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS
     
     // Verifico condiciones
     if (inputData.ncol() < 2) {
       stop("Please verify the input matrix");
     }
     if (initCond.size() < 3) {
       stop("You must support the relative area of the glacier");
     } 
     if (param.size() < 6){
       stop("Please verify the parameter vector");
     }
     
     // Genero el vector de salida
     int n = inputData.nrow(); // número filas
     int m = 9;                // número de columnas
     NumericMatrix out(n, m);
     
     // Declaro variables que voy a utilizar y nombre de parámetros
     double Prain, Psnow, SWE, Msnow, Mice, Mtot, Cum, Total, TotScal;
     double SFCF, Tt, Tm, fm, fic;
     
     // Asigno valores a los parámetros
     SFCF = param[0];
     Tt   = param[1];
     Tm   = param[2];
     fm   = param[3];
     fic  = param[5];
     
     // Corro rutina nivo-glaciar
     for (int i = 0; i < n; ++i){
       
       // Precipitación líquida o sólida
       if (inputData(i, 0) > Tt){
         Prain = inputData(i, 1) * SFCF;
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
           Mice    = (inputData(i, 0) - Tm) * fic;
           Mtot    = Msnow + Mice;
           SWE    += Psnow - Msnow;
           Cum     = Psnow - Mtot;
           Total   = (Mtot + Prain); 
           TotScal = (Mtot + Prain) * initCond[2]; 
           
         } else { //hay nieve
           
           Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
           Mice    = 0.0;
           Mtot    = Msnow + Mice;
           SWE    += Psnow - Msnow;
           Cum     = Psnow - Mtot;
           Total   = (Mtot + Prain); 
           TotScal = (Mtot + Prain) * initCond[2]; 
         }
       } else { //temperatura del aire MENOR a Tmelt
         
         Msnow   = 0.0;
         Mice    = 0.0;
         Mtot    = Msnow + Mice;
         SWE    += Psnow - Msnow;
         Cum     = Psnow - Mtot;
         Total   = (Mtot + Prain); 
         TotScal = (Mtot + Prain) * initCond[2]; 
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
     
    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }

  } else if (model == 2) {
    // MODELO TI CON SCA
    
    // GLACIAR DESCUBIERTO, SUELO O GLACIAR CUBIERTO
    
    if (initCond[1] == 1) {
      // SUPERFICIE GLACIAR
      
      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() == 2) {
        stop("You must support the relative area of the glacier");
      } 
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }
      
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
          Prain = inputData(i, 1) * SFCF;
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
            TotScal = (Mtot + Prain) * initCond[2]; 
            
          } else { //hay nieve
            
            Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
            Mice    = 0.0;
            Mtot    = Msnow + Mice;
            SWE    += Psnow - Msnow;
            Cum     = Psnow - Mtot;
            Total   = (Mtot + Prain); 
            TotScal = (Mtot + Prain) * initCond[2]; 
          }
        } else { //temperatura del aire MENOR a Tmelt
          
          Msnow   = 0.0;
          Mice    = 0.0;
          Mtot    = Msnow + Mice;
          SWE    += Psnow - Msnow;
          Cum     = Psnow - Mtot;
          Total   = (Mtot + Prain); 
          TotScal = (Mtot + Prain) * initCond[2]; 
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
      
    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO
      
      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      } 
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }
      
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
          Prain = inputData(i, 1) * SFCF; 
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
      
    } else if (initCond[1] == 3) {
      
      // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS
      
      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 3) {
        stop("You must support the relative area of the glacier");
      } 
      if (param.size() < 6){
        stop("Please verify the parameter vector");
      }
      
      // Genero el vector de salida
      int n = inputData.nrow(); // número filas
      int m = 9;                // número de columnas
      NumericMatrix out(n, m);
      
      // Declaro variables que voy a utilizar y nombre de parámetros
      double Prain, Psnow, SWE, Msnow, Mice, Mtot, Cum, Total, TotScal;
      double SFCF, Tt, Tm, fm, fic;
      
      // Asigno valores a los parámetros
      SFCF = param[0];
      Tt   = param[1];
      Tm   = param[2];
      fm   = param[3];
      fic  = param[5];
      
      // Corro rutina nivo-glaciar
      for (int i = 0; i < n; ++i){
        
        // Precipitación líquida o sólida
        if (inputData(i, 0) > Tt){
          Prain = inputData(i, 1) * SFCF;
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
            Mice    = (inputData(i, 0) - Tm) * fic;
            Mtot    = Msnow + Mice;
            SWE    += Psnow - Msnow;
            Cum     = Psnow - Mtot;
            Total   = (Mtot + Prain); 
            TotScal = (Mtot + Prain) * initCond[2]; 
            
          } else { //hay nieve
            
            Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
            Mice    = 0.0;
            Mtot    = Msnow + Mice;
            SWE    += Psnow - Msnow;
            Cum     = Psnow - Mtot;
            Total   = (Mtot + Prain); 
            TotScal = (Mtot + Prain) * initCond[2]; 
          }
        } else { //temperatura del aire MENOR a Tmelt
          
          Msnow   = 0.0;
          Mice    = 0.0;
          Mtot    = Msnow + Mice;
          SWE    += Psnow - Msnow;
          Cum     = Psnow - Mtot;
          Total   = (Mtot + Prain); 
          TotScal = (Mtot + Prain) * initCond[2]; 
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
      
      
    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }
    
  } else if (model == 3) {
    // MODELO TI CON GCA
    
    // SUELO O GLACIAR
    
    if (initCond[1] == 1) {
      
      // SUPERFICIE GLACIAR
      
      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      } 
      if (param.size() < 5){
        stop("Please verify the parameter vector");
      }
      
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
          Prain = inputData(i, 1) * SFCF;
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
      
    } else if (initCond[1] == 2) {
      // SUPERFICIE SUELO
      
      // Verifico condiciones
      if (inputData.ncol() < 2) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      } 
      if (param.size() < 4){
        stop("Please verify the parameter vector");
      }
      
      // Genero el vector de salida
      int n = inputData.nrow(); // número filas
      int m = 5;                // número de columnas
      NumericMatrix out(n, m);
      
      // Declaro variables que voy a utilizar y nombre de parámetros
      double Prain, Psnow, SWE, Msnow, Total;
      double SFCF, Tt, Tm, fm;
      
      // Asigno valores a los parámetros
      SFCF = param[0];
      Tt   = param[1];
      Tm   = param[2];
      fm   = param[3];
      
      // Corro rutina nival
      for (int i = 0; i < n; ++i){
        
        // Precipitación líquida o sólida
        if (inputData(i, 0) > Tt){
          Prain = inputData(i, 1) * SFCF; 
          Psnow = 0.0;
        } else {
          Prain = 0.0;
          Psnow = inputData(i, 1) * SFCF;
        }
        
        // Nieve derretida
        if (i == 0) {SWE = initCond[0];} //me aeguro el condicional para la primer corrida
        if (inputData(i, 0) > Tm) {//temperatura del aire MAYOR a Tmelt
          
          if (SWE == 0.0) { //no hay nieve
            
            Msnow = 0.0;
            SWE  += Psnow - Msnow;
            Total = (Msnow + Prain); 
            
          } else { //hay nieve
            
            Msnow = std::min( (inputData(i, 0) - Tm) * fm, SWE);
            SWE  += Psnow - Msnow;
            Total = (Msnow + Prain); 
          }
        } else {//temperatura del aire MENOR a Tmelt
          
          Msnow = 0.0;
          SWE  += Psnow - Msnow;
          Total = (Msnow + Prain); 
          
        }
        
        // Relleno matriz de salida
        out(i, 0) = Prain;
        out(i, 1) = Psnow;
        out(i, 2) = SWE;
        out(i, 3) = Msnow;
        out(i, 4) = Total;
        
      }
      
      colnames(out) = CharacterVector::create("Prain", "Psnow", "SWE", "Msnow", "Total");
      return out;
      
    } else if (initCond[1] == 3) {
      
      // SUPERFICIE GLACIAR CUBIERTA CON DEBRIS
      
      // Verifico condiciones
      if (inputData.ncol() < 3) {
        stop("Please verify the input matrix");
      }
      if (initCond.size() < 2) {
        stop("Please verify the initCond argument");
      } 
      if (param.size() < 6){
        stop("Please verify the parameter vector");
      }
      
      // Genero el vector de salida
      int n = inputData.nrow(); // número filas
      int m = 9;                // número de columnas
      NumericMatrix out(n, m);
      
      // Declaro variables que voy a utilizar y nombre de parámetros
      double Prain, Psnow, SWE, Msnow, Mice, Mtot, Cum, Total, TotScal;
      double SFCF, Tt, Tm, fm, fic;
      
      // Asigno valores a los parámetros
      SFCF = param[0];
      Tt   = param[1];
      Tm   = param[2];
      fm   = param[3];
      fic  = param[5];
      
      // Corro rutina nivo-glaciar
      for (int i = 0; i < n; ++i){
        
        // Precipitación líquida o sólida
        if (inputData(i, 0) > Tt){
          Prain = inputData(i, 1) * SFCF;
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
            Mice    = (inputData(i, 0) - Tm) * fic;
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
     
      
    } else {
      stop("initCond[2] must be 1, 2 or 3");
    }
    
  } else {
    stop("Model not avilable");
  }

} // FIN FUNCIÓN
