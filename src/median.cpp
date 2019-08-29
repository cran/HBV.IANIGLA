#include <Rcpp.h>
using namespace Rcpp;

// Función auxiliar para modelo 1 UH

// [[Rcpp::export]]
int medianCpp(double x){
  double n = ceil(x);
  NumericVector y(n);
  int out;

  y[0] = 1;
  for (int i = 1; i < n; i++){
    y[i] = y[i - 1] + 1;
  }

  out = median(y);

  return out;
}
