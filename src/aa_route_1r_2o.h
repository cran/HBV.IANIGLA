#include <Rcpp.h>
#ifndef ROUTE_1R_2O_H
#define ROUTE_1R_2O_H

Rcpp::NumericMatrix route_1r_2o(Rcpp::NumericMatrix inputData,
                                Rcpp::NumericVector initCond,
                                Rcpp::NumericVector param);
#endif
