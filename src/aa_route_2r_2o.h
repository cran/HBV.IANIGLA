#include <Rcpp.h>
#ifndef ROUTE_2R_2O_H
#define ROUTE_2R_2O_H

Rcpp::NumericMatrix route_2r_2o(bool lake,
                                Rcpp::NumericMatrix inputData,
                                Rcpp::NumericVector initCond,
                                Rcpp::NumericVector param);
#endif
