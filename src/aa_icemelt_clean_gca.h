#include <Rcpp.h>
#ifndef ICEMELT_CLEAN_GCA
#define ICEMELT_CLEAN_GCA

Rcpp::NumericMatrix icemelt_clean_gca(Rcpp::NumericMatrix inputData,
                                      Rcpp::NumericVector initCond,
                                      Rcpp::NumericVector param);
#endif
