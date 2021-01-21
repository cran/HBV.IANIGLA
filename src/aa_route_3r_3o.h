#include <Rcpp.h>
#ifndef ROUTE_3R_3O_H
#define ROUTE_3R_3O_H

Rcpp::NumericMatrix route_3r_3o(bool lake,
                                Rcpp::NumericMatrix inputData,
                                Rcpp::NumericVector initCond,
                                Rcpp::NumericVector param);
#endif
