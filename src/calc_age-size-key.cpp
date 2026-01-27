#include <Rcpp.h>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"
#include "ptnorm_trunc.h"

// [[Rcpp::export]]
Rcpp::NumericVector CalcAgeSizeKey_(
    const Rcpp::NumericMatrix& MeanAtAge, // [nAge, nTS]
    const Rcpp::NumericMatrix& SDatAge,   // [nAge, nTS]
    const Rcpp::NumericVector& Classes,   // [nClass]
    const double TruncSD,
    const std::string& Dist) {
  
  const int nAge   = MeanAtAge.nrow();
  const int nTS    = MeanAtAge.ncol();
  const int nClass = Classes.length();
  
  // ---- Checks
  if (SDatAge.nrow() != nAge || SDatAge.ncol() != nTS) {
    Rcpp::stop("`MeanAtAge` and `SDatAge` must have identical dimensions");
  }
  
  if (nClass < 2) {
    Rcpp::stop("Classes must have length >= 2");
  }
  
  const bool is_lognormal = (Dist == "lognormal");
  
  // ---- output array [nAge, nClass, nTS]
  Rcpp::NumericVector ASK(nAge * nClass * nTS);
  ASK.attr("dim") = Rcpp::IntegerVector::create(nAge, nClass, nTS);
  
  Rcpp::NumericVector mu(nAge);
  Rcpp::NumericVector sd(nAge);
  Rcpp::NumericVector classLower(nClass);
  
  // ---- Class lower bounds
  const double By = Classes[1] - Classes[0];
  if (!std::isfinite(By) || By <= 0.0) {
    Rcpp::stop("Classes must be increasing and evenly spaced");
  }
  
  for (int k = 0; k < nClass; ++k) {
    double cl = Classes[k] - 0.5 * By;
    if (is_lognormal) {
      cl = std::log(cl);
    }
    classLower[k] = cl;
  }
  
  
  for (int ts = 0; ts < nTS; ++ts) {
    for (int a = 0; a < nAge; ++a) {
      mu[a] = MeanAtAge(a, ts);
      sd[a] = SDatAge(a, ts);
    }
    
    // Lognormal transform
    if (is_lognormal) {
      for (int a = 0; a < nAge; ++a) {
        
        double mui = mu[a];
        double sdi = sd[a] / mui;
        
        if (!std::isfinite(sdi) || sdi <= 0.0) {
          sdi = 0.05;
        }
        
        double mui_log = std::log(mui) - 0.5 * sdi * sdi;
        if (!std::isfinite(mui_log)) {
          mui_log = std::log(1e-6);
        }
        
        mu[a] = mui_log;
        sd[a] = sdi;
      }
    }
    
    // ---- First class
    Rcpp::NumericVector p_first = ptnorm_trunc(classLower[1], mu, sd, TruncSD);
    for (int a = 0; a < nAge; ++a) {
      ASK[a + nAge * (0 + nClass * ts)] = p_first[a];
    }
    
    // ---- Middle classes
    Rcpp::NumericVector prev = ptnorm_trunc(classLower[1], mu, sd, TruncSD);
    for (int k = 1; k < nClass - 1; ++k) {
      Rcpp::NumericVector curr = ptnorm_trunc(classLower[k + 1], mu, sd, TruncSD);
      for (int a = 0; a < nAge; ++a) {
        ASK[a + nAge * (k + nClass * ts)] = curr[a] - prev[a];
      }
      prev = curr; 
    }
    
    // ---- Last class
    Rcpp::NumericVector p_last = ptnorm_trunc(classLower[nClass - 1], mu, sd, TruncSD);
    for (int a = 0; a < nAge; ++a) {
      ASK[a + nAge * ((nClass - 1) + nClass * ts)] = 1.0 - p_last[a];
    }
    
  }
  
  return ASK;
}