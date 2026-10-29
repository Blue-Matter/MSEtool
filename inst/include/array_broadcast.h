// array_broadcast.h
#ifndef MSEtool_ARRAY_BROADCAST_H
#define MSEtool_ARRAY_BROADCAST_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>

// Elementwise operatinon between two arrays whose dims are pairwise equal or 1
// (stride-0 broadcast), computed in a single pass with a single allocation
// (the output). op: 0 = +, 1 = -, 2 = *, 3 = / 
inline Rcpp::NumericVector ArrayBroadcastOp(
    const Rcpp::NumericVector& x, const Rcpp::IntegerVector& dimx,
    const Rcpp::NumericVector& y, const Rcpp::IntegerVector& dimy,
    int op) {

  const int nd = dimx.size();
  std::vector<long> outdim(nd);
  for (int d = 0; d < nd; d++)
    outdim[d] = std::max(dimx[d], dimy[d]);

  std::vector<long> sx(nd), sy(nd);
  long accx = 1, accy = 1, accout = 1;
  for (int d = 0; d < nd; d++) {
    sx[d] = (dimx[d] == 1) ? 0 : accx;
    sy[d] = (dimy[d] == 1) ? 0 : accy;
    accx  *= dimx[d];
    accy  *= dimy[d];
    accout *= outdim[d];
  }

  const long total = accout;
  Rcpp::NumericVector out(total);

  std::vector<long> idx(nd, 0);
  long ox = 0, oy = 0;
  const double* xp = x.begin();
  const double* yp = y.begin();
  double* op_out = out.begin();

  for (long i = 0; i < total; i++) {
    const double a = xp[ox];
    const double b = yp[oy];
    double r;
    switch (op) {
      case 0: r = a + b; break;
      case 1: r = a - b; break;
      case 2: r = a * b; break;
      case 3: r = a / b; break;
      default: r = a * b;
    }
    op_out[i] = r;

    // odometer increment with incremental offset update
    for (int d = 0; d < nd; d++) {
      idx[d]++;
      ox += sx[d];
      oy += sy[d];
      if (idx[d] < outdim[d]) break;
      ox -= sx[d] * outdim[d];
      oy -= sy[d] * outdim[d];
      idx[d] = 0;
    }
  }

  Rcpp::IntegerVector outdim_i(nd);
  for (int d = 0; d < nd; d++) outdim_i[d] = (int)outdim[d];
  out.attr("dim") = outdim_i;
  return out;
}

#endif
