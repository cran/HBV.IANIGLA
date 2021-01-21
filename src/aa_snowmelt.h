#include <Rcpp.h>
#ifndef SNOWMELT
#define SNOWMELT

Rcpp::NumericMatrix snowmelt(Rcpp::NumericMatrix inputData,
                             Rcpp::NumericVector initCond,
                             Rcpp::NumericVector param);
#endif
