#ifndef SRR_FUNCTIONS_H
#define SRR_FUNCTIONS_H

#include <Rcpp.h>
#include <cmath>
#include "srr_functions.h"

using namespace Rcpp;

inline double BevertonHolt_kernel(
    double S,
    double S0,
    double R0,
    double h) {
  
  if (S <= 0.0 || S0 <= 0.0 || R0 <= 0.0)
    return 0.0;
  
  const double phi0  = S0 / R0;
  const double alpha = 4.0 * h / ((1.0 - h) * phi0);
  const double beta  = (5.0 * h - 1.0) / ((1.0 - h) * phi0 * R0);
  return alpha * S / (1.0 + beta * S);
} 


inline double Ricker_kernel(
    double S,
    double S0,
    double R0,
    double hR) {
  
  if (S <= 0.0 || S0 <= 0.0 || R0 <= 0.0)
    return 0.0;
  
  const double phi0 = S0 / R0;
  
  const double x = std::pow(5.0 * hR, 1.25);
  const double alpha = x / phi0;
  const double beta = std::log(x) / (phi0 * R0);
  return alpha * S * std::exp(-beta * S);
} 

inline double HockeyStick_kernel(
    double S,
    double S0,
    double R0,
    double Shinge) {
  
  if (S <= 0.0 || S0 <= 0.0 || R0 <= 0.0)
    return 0.0;
  
  const double S_hinge = S0 * Shinge;
  const double coef    = R0 / (2.0 * S_hinge);
  const double expR = coef * ((S + S_hinge) - std::abs(S - S_hinge));
  return expR > 0.0 ? expR : 0.0;
} 


#endif