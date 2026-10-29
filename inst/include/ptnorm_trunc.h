#ifndef CALC_PTNORM_TRUNC_H
#define CALC_PTNORM_TRUNC_H

#include <Rcpp.h>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline Rcpp::NumericVector ptnorm_trunc(
    double q,
    const Rcpp::NumericVector& mu,
    const Rcpp::NumericVector& sd,
    double truncsd) {
  
  const int n = mu.length();
  Rcpp::NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    
    const double mui = mu(i);
    const double sdi = sd(i);
    
    if (!std::isfinite(sdi) || sdi <= 0.0) {
      out(i) = (q >= mui) ? 1.0 : 0.0;
      continue;
    }
    
    const double a = mui - truncsd * sdi;
    const double b = mui + truncsd * sdi;
    if (q <= a) {
      out(i) = 0.0;
    } else if (q >= b) {  
      out(i) = 1.0;
    } else { 
      const double p1 = R::pnorm(q, mui, sdi, 1, 0);
      const double p2 = R::pnorm(a, mui, sdi, 1, 0);
      const double p3 = R::pnorm(b, mui, sdi, 1, 0);
       
      out(i) = (p3 > p2) ? (p1 - p2) / (p3 - p2) : 0.0;
    }
  }  
  return(out);
}  

#endif