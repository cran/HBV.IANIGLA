#include <Rcpp.h>
#ifndef ICEMELT_DEBRIS_GCA
#define ICEMELT_DEBRIS_GCA

Rcpp::NumericMatrix icemelt_debris_gca(Rcpp::NumericMatrix inputData,
                                      Rcpp::NumericVector initCond,
                                      Rcpp::NumericVector param);
#endif
