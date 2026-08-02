#ifndef SRR_FUNCTIONS_H
#define SRR_FUNCTIONS_H

#include <Rcpp.h>
#include <cmath>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

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

// SRR model codes
constexpr int SRR_BEVERTON_HOLT = 0;
constexpr int SRR_RICKER        = 1;
constexpr int SRR_HOCKEY_STICK  = 2;


inline double EvalSRR(
    int model,
    double S,
    double S0,
    double R0,
    const std::vector<ConstArrayView2D>& pars_st,
    int sim,   
    int y) {
  
  if (pars_st.empty()) {
    Rcpp::stop("SRR model called with zero parameters" + std::to_string(model));
  }
  
  switch (model) {

  case SRR_BEVERTON_HOLT: {
    const int sim_h = sim_index<2>(sim, pars_st[0]);
    return BevertonHolt_kernel(
      S,
      S0,
      R0,
      pars_st[0](sim_h, y) // h
    );
  }

  case SRR_RICKER: {
    const int sim_hR = sim_index<2>(sim, pars_st[0]);
    return Ricker_kernel(
      S,
      S0,
      R0,
      pars_st[0](sim_hR, y) // hR
    );
  }

  
  case SRR_HOCKEY_STICK: {
    const int sim_Sh = sim_index<2>(sim, pars_st[0]);
    return HockeyStick_kernel(
      S,
      S0,
      R0,
      pars_st[0](sim_Sh, y) // Shinge
    );
  }
 
  default: 
    Rcpp::stop("Unknown SRR model code");
  }
}  
#endif