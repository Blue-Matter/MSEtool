#include <Rcpp.h>
#include <cmath>
#include "srr_functions.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector BevertonHolt_cpp(
    const NumericVector& S,
    const double S0,
    const double R0,
    const double h) {
  
  const int n = S.size();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = BevertonHolt_kernel(S[i], S0, R0, h);
  } 
   
  return out;
} 


// [[Rcpp::export]]
NumericVector Ricker_cpp(
    const NumericVector& S,
    const double S0,
    const double R0,
    const double hR) {
  
  const int n = S.size();
  NumericVector out(n);
  // const double phi0 = S0 / R0;
  //  
  // const double x = std::pow(5.0 * hR, 1.25);
  // const double alpha = x / phi0;
  // const double beta = std::log(x) / (phi0 * R0);
  
  for (int i = 0; i < n; ++i) {
    out[i] = Ricker_kernel(S[i], S0, R0, hR);
  } 
  return out;
} 


// [[Rcpp::export]]
NumericVector HockeyStick_cpp(
    const NumericVector& S,
    const double S0,
    const double R0,
    const double Shinge) {

  if (Shinge <= 0.0 || Shinge > 1.0) {
    Rcpp::stop("Shinge must be in (0, 1]");
  }
  
  const int n = S.size();
  NumericVector out(n);
   
  for (int i = 0; i < n; ++i) {
    out[i] = HockeyStick_kernel(S[i], S0, R0, Shinge);
  } 
  
  return out;
} 

