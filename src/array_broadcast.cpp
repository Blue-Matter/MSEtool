#include <Rcpp.h>
#include "array_broadcast.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ArrayBroadcastOp_(NumericVector x, IntegerVector dimx,
                                 NumericVector y, IntegerVector dimy,
                                 int op) {
  return ArrayBroadcastOp(x, dimx, y, dimy, op);
}
