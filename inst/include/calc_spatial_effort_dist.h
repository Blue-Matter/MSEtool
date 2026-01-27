#ifndef CALC_SPATIAL_DISTRIBUTION_H
#define CALC_SPATIAL_DISTRIBUTION_H

#include <Rcpp.h>
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Calculate spatial effort distribution across areas
inline void CalcSpatialDistribution(
    const int y,    
    const int nSim,
    Array4D& Distribution,         
    const std::vector<Array4D>& Number,    
    const std::vector<ConstArrayView4D>& WeightFleet,   
    const std::vector<ConstArrayView5D>& SelAge,
    const std::vector<ConstArrayView5D>& RetAge,
    const  ConstArrayView4D& q,                         
    const  ConstArrayView5D& Closure,                    
    const  ConstArrayView3D& Targeting,                
    const  Array3D& Effort,                  
    const  ConstArrayView2D& RelSize,
    const int nStock,
    const int nFleet,
    const int nArea) {                   
  
  // ---------------
  // Checks
  // ---------------
  
  if (nStock < 1)
    Rcpp::stop("nStock < 1");
  
  if (nFleet < 1)
    Rcpp::stop("nFleet < 1");
  
  if (nArea < 1)
    Rcpp::stop("nArea < 1");
  
  // Distribution
  if (y < 0 || y >= Distribution.dim[1])
    Rcpp::stop("Distribution: y out of bounds");
  
  if (Distribution.dim[0] != nSim)
    Rcpp::stop("Distribution nSim mismatch");
  
  if (Distribution.dim[2] != nFleet)
    Rcpp::stop("Distribution nFleet mismatch");
  
  if (Distribution.dim[3] != nArea)
    Rcpp::stop("Distribution nArea mismatch");
  
  // q
  if (q.dim[0] != nSim)
    Rcpp::stop("q nSim mismatch");

  if (q.dim[1] != nStock)
    Rcpp::stop("q nStock mismatch");
  
  if (y < 0 || y >= q.dim[2])
    Rcpp::stop("q: y out of bounds");

  if (q.dim[3] != nFleet)
    Rcpp::stop("q nFleet mismatch");
  
  // Closure
  if (Closure.dim[0] != nSim)
    Rcpp::stop("Closure nSim mismatch");
 
  if (Closure.dim[1] != nStock)
    Rcpp::stop("Closure nStock mismatch");
 
  if (y < 0 || y >= Closure.dim[2])
    Rcpp::stop("Closure: y out of bounds");

  if (Closure.dim[3] != nFleet)
    Rcpp::stop("Closure nFleet mismatch");

  if (Closure.dim[4] != nArea)
    Rcpp::stop("Closure nArea mismatch");
  // Targeting
  if (Targeting.dim[0] != nSim)
    Rcpp::stop("Targeting nSim mismatch");
 
  if (y < 0 || y >= Targeting.dim[1])
    Rcpp::stop("Targeting: y out of bounds");
  if (Targeting.dim[2] != nFleet)
    
    Rcpp::stop("Targeting nFleet mismatch");
 
  // Effort
  if (Effort.dim[0] != nSim)
    Rcpp::stop("Effort nSim mismatch");

  if (y < 0 || y >= Effort.dim[1])
    Rcpp::stop("Effort: y out of bounds");

  if (Effort.dim[2] != nFleet)
    Rcpp::stop("Effort nFleet mismatch");

  // RelSize
  if (RelSize.dim[0] != nSim)
    Rcpp::stop("RelSize nSim mismatch");

  if (RelSize.dim[1] != nArea)
    Rcpp::stop("RelSize nArea mismatch");

  
  if ((int)Number.size() < nStock ||
      (int)WeightFleet.size() < nStock ||
      (int)SelAge.size() < nStock ||
      (int)RetAge.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  
  // Single-area shortcut
  if (nArea == 1) {
    for (int sim = 0; sim < nSim; ++sim)
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }

  // Util array: sim, fleet, area 
  const std::array<int,3> dim = {nSim, nFleet, nArea};
  Array3D Util(dim, 0.0);

  
  // Loop over stocks
  for (int st = 0; st < nStock; ++st) { 
    Array3D B_hat(dim, 0.0);
    
    const Array4D& Num_st = Number[st];
    const ConstArrayView4D& Wgt_st = WeightFleet[st];
    const ConstArrayView5D& Sel_st = SelAge[st];
    const ConstArrayView5D& Ret_st = RetAge[st];
    
    const int nAge = Num_st.dim[1];
    
    if (Num_st.dim[0] != nSim)
      Rcpp::stop("Number[[%d]] nSim mismatch", st+1);
    
    if (Num_st.dim[3] != nArea)
      Rcpp::stop("Number[[%d]] nArea mismatch", st+1);
    
    if (Wgt_st.dim[0] != nSim || Wgt_st.dim[3] != nFleet)
      Rcpp::stop("WeightFleet[[%d]] dim mismatch", st+1);
    
    if (Sel_st.dim[0] != nSim || Sel_st.dim[3] != nFleet || Sel_st.dim[4] != nArea)
      Rcpp::stop("SelAge[[%d]] dim mismatch", st+1);
    
    if (Ret_st.dim[1] != Num_st.dim[1])
      Rcpp::stop("RetAge[[%d]] age mismatch", st+1);
    
    if (Wgt_st.dim[1] != nAge ||
        Sel_st.dim[1] != nAge ||
        Ret_st.dim[1] != nAge)
      Rcpp::stop("Age dimension mismatch in stock %d", st+1);
    
    
    // Exploitable biomass per unit effort
    for (int sim = 0; sim < nSim; ++sim) {

      for (int fl = 0; fl < nFleet; ++fl) {
        for (int ar = 0; ar < nArea; ++ar) {
          
          if (Closure(sim, st, y, fl, ar) <= 0.0) continue;
          double B_sfr = 0.0;
          for (int age = 0; age < nAge; ++age) {
            B_sfr +=
              Num_st(sim, age, y, ar) *
              Wgt_st(sim, age, y, fl) *
              Sel_st(sim, age, y, fl, ar) *
              Ret_st(sim, age, y, fl, ar);
          }
          B_hat(sim, fl, ar) = q(sim, st, y, fl) * B_sfr;
        } 
      }
    }
    
    // Within-season saturation 
    for (int sim = 0; sim < nSim; ++sim) {
      
      for (int fl = 0; fl < nFleet; ++fl) {
        const double phi = q(sim, st, y, fl) * Effort(sim, y, fl);
        
        // Median B_ref across areas
        std::vector<double> Bvec(nArea);
        for (int ar = 0; ar < nArea; ++ar)
          Bvec[ar] = B_hat(sim, fl, ar);
        
        std::nth_element(
          Bvec.begin(),
          Bvec.begin() + nArea / 2,
          Bvec.end()
        ); 
        const double Bref = Bvec[nArea / 2];
        
        if (Bref <= 0.0) continue;
        
        // Apply fleet-specific utility
        for (int ar = 0; ar < nArea; ++ar) {
          const double B = B_hat(sim, fl, ar);
          const double A = RelSize(sim, ar);
          
          if (A > 0.0 && B > 0.0 && Bref > 0.0) {
            const double Gamma = B / (A * Bref);
            Util(sim, fl, ar) += B / (1.0 + phi * Gamma);
          }
        }
      }
    }
  } // end stock loop
  
  // Normalize utility across areas 
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total += Util(sim, fl, ar);
      
      if (total > 0.0) {
        for (int ar = 0; ar < nArea; ++ar)
          Util(sim, fl, ar) /= total;
      } 
    }
  }
  
  // Calculate Effort Distribution
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      const double theta = Targeting(sim,y,fl);
      if (theta <= 0.0) continue;
      
      double total = 0.0;
      
      // cache Util^theta
      std::vector<double> UtilTheta(nArea);
      for (int ar = 0; ar < nArea; ++ar) {
        const double u = Util(sim, fl, ar);
        if (u > 0.0) {
          const double ut = std::pow(std::max(u, 1e-12), theta);
          UtilTheta[ar] = ut;
          total += ut;
        } else { 
          UtilTheta[ar] = 0.0;
        }
      }
      
      if (total > 0.0) {
        const double inv_total = 1.0 / total;
        for (int ar = 0; ar < nArea; ++ar) {
          if (Distribution(sim, y, fl, ar) <= 1E-6) { // skips if users provide values
            Distribution(sim, y, fl, ar) = 0.0;
            Distribution(sim, y, fl, ar) = UtilTheta[ar] * inv_total;  
          }
          
        }
      }  
    }
  }
  
}

#endif // CALC_SPATIAL_DISTRIBUTION_H