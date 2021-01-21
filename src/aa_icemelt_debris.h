#include <Rcpp.h>
#ifndef ICEMELT_DEBRIS
#define ICEMELT_DEBRIS

Rcpp::NumericMatrix icemelt_debris(Rcpp::NumericMatrix inputData,
                                   Rcpp::NumericVector initCond,
                                   Rcpp::NumericVector param);
#endif
