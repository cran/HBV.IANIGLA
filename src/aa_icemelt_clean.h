#include <Rcpp.h>
#ifndef ICEMELT_CLEAN
#define ICEMELT_CLEAN

Rcpp::NumericMatrix icemelt_clean(Rcpp::NumericMatrix inputData,
                                  Rcpp::NumericVector initCond,
                                  Rcpp::NumericVector param);
#endif
