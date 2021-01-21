#include <Rcpp.h>
#ifndef SNOWMELT_SCA
#define SNOWMELT_SCA

Rcpp::NumericMatrix snowmelt_sca(Rcpp::NumericMatrix inputData,
                                 Rcpp::NumericVector initCond,
                                 Rcpp::NumericVector param);
#endif
