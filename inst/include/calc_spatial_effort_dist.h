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
    const std::vector<int>& Sims,
    const int nSim,
    Array4D& Distribution,                                // sim, year, fleet, area
    const std::vector<Array4D>& Number,                   // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& WeightFleet,     // [stock] sim, age, year, fleet  
    const std::vector<ConstArrayView5D>& SelAge,          // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& RetAge,          // [stock] sim, age, year, fleet, area
    const  ConstArrayView4D& q,                           // sim, stock, year, fleet          
    const  ConstArrayView5D& Closure,                     // sim, stock, year, fleet, area       
    const  ConstArrayView3D& Targeting,                   // sim, year, fleet
    const  Array3D& Effort,                               // sim, year, fleet
    const  ConstArrayView2D& RelSize,                     // sim, area
    const int nStock,
    const int nFleet,
    const int nArea) {                   
  
  // ---------------
  // Checks
  // ---------------
  
  if ((int)Number.size() < nStock ||
      (int)WeightFleet.size() < nStock ||
      (int)SelAge.size() < nStock ||
      (int)RetAge.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  check_dims<4>(Distribution, {nSim, Distribution.dim[1], nFleet, nArea}, "Distribution", y, 1);
  check_dims<4>(q, {nSim, nStock, q.dim[2], nFleet}, "q", y, 2);
  check_dims<5>(Closure, {nSim, nStock, Closure.dim[2], nFleet, nArea}, "Closure", y, 2);
  check_dims<3>(Targeting, {nSim, Targeting.dim[1], nFleet}, "Targeting", y, 1);
  check_dims<3>(Effort, {nSim, Effort.dim[1], nFleet}, "Effort", y, 1);
  check_dims<2>(RelSize, {nSim, nArea}, "RelSize");
  
  // Single-area shortcut
  if (nArea == 1) {
    for (int sim : Sims) 
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }
  
  // Util array: sim, fleet, area 
  const std::array<int,3> dim = {nSim, nFleet, nArea};
  Array3D Util(dim, 0.0);
  
  // Loop over stocks
  Array3D B_hat(dim, 0.0);
  for (int st = 0; st < nStock; ++st) { 
    std::fill(B_hat.x.begin(), B_hat.x.end(), 0.0);
    
    const Array4D& Num_st = Number[st];                 // sim, age, year, area
    const ConstArrayView4D& Wgt_st = WeightFleet[st];   // sim, age, year, fleet
    const ConstArrayView5D& Sel_st = SelAge[st];        // sim, age, year, fleet, area
    const ConstArrayView5D& Ret_st = RetAge[st];        // sim, age, year, fleet, area
    
    const int nAge = Num_st.dim[1];
    
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    check_dims<4>(Wgt_st, {nSim, nAge, Wgt_st.dim[2], nFleet}, "WeightFleet", y, 2);
    check_dims<5>(Sel_st, {nSim, nAge, Sel_st.dim[2], nFleet, nArea}, "SelAge", y, 2);
    check_dims<5>(Ret_st, {nSim, nAge, Ret_st.dim[2], nFleet, nArea}, "RetAge", y, 2);
    
    // Exploitable biomass per unit effort
    for (int sim : Sims) {
      const int sim_cl  = sim_index<4>(sim, Closure, "Closure");
      const int sim_wgt = sim_index<4>(sim, Wgt_st, "Wgt_st");
      const int sim_sel = sim_index<5>(sim, Sel_st, "Sel_st");
      const int sim_ret = sim_index<5>(sim, Ret_st, "Ret_st");
      const int sim_q   = sim_index<4>(sim, q, "q");
      
      for (int fl = 0; fl < nFleet; ++fl) {
        for (int ar = 0; ar < nArea; ++ar) {
          if (Closure(sim_cl, st, y, fl, ar) <= 0.0) continue;
          double B_sfr = 0.0;
          const double q_val = q(sim_q, st, y, fl);
          if (q_val <= 0.0) continue; 
          
          for (int age = 0; age < nAge; ++age) {
            B_sfr +=
              Num_st(sim, age, y, ar) *
              Wgt_st(sim_wgt, age, y, fl) *
              Sel_st(sim_sel, age, y, fl, ar) *
              Ret_st(sim_ret, age, y, fl, ar);
          }  
          B_hat(sim, fl, ar) =q_val * B_sfr;
        } 
      }
    }  
    
    // Within-season saturation 
    for (int sim : Sims) {
      
      for (int fl = 0; fl < nFleet; ++fl) {
        const int sim_q = sim_index<4>(sim, q, "q");
        const double phi = q(sim_q, st, y, fl) * Effort(sim, y, fl);
        
        // Median B_ref across areas
        std::vector<double> Bvec(nArea);
        for (int ar = 0; ar < nArea; ++ar)
          Bvec[ar] = B_hat(sim, fl, ar);
        
        // Median B_ref across non-zero areas only
        // Using all areas causes Bref=0 when majority of areas have zero B_hat
        // (e.g. closed areas or zero retention), which incorrectly zeroes Util
        // for areas that do have exploitable biomass
        std::vector<double> nonzero;
        nonzero.reserve(nArea);
        for (int ar = 0; ar < nArea; ++ar) {
          if (B_hat(sim, fl, ar) > 0.0)
            nonzero.push_back(B_hat(sim, fl, ar));
        } 
        
        if (nonzero.empty()) continue;
        
        std::nth_element(nonzero.begin(), nonzero.begin() + nonzero.size()/2, nonzero.end());
        const double Bref = nonzero[nonzero.size()/2];
        
        if (Bref <= 0.0) continue;
        
        // Apply fleet-specific utility
        for (int ar = 0; ar < nArea; ++ar) {
          const double B = B_hat(sim, fl, ar);
          const int sim_rs = sim_index<2>(sim, RelSize, "RelSize");
          const double A = RelSize(sim_rs, ar);
          
          if (A > 0.0 && B > 0.0 && Bref > 0.0) {
            const double Gamma = B / (A * Bref);
            Util(sim, fl, ar) += B / (1.0 + phi * Gamma);
          }
        }
      }
    }
  } // end stock loop  
  
  // Normalize utility across areas 
  for (int sim : Sims) {
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
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      const int sim_t = sim_index<3>(sim, Targeting, "Targeting");
      const double theta = Targeting(sim_t, y, fl);
      
      if (theta <= 0.0) {
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = 0.0;
        } 
        continue;
      } 
      double total = 0.0;
      
      // cache Util^theta
      std::vector<double> UtilTheta(nArea);
      for (int ar = 0; ar < nArea; ++ar) {
        const double u = Util(sim, fl, ar);
        if (u > 0.0) {
          const double ut = (theta == 1.0) ? u : std::pow(std::max(u, 1e-12), theta);
          UtilTheta[ar] = ut;
          total += ut;
        } else {           UtilTheta[ar] = 0.0;
        }
      }
       
      if (total > 0.0) {
        const double inv_total = 1.0 / total;
        for (int ar = 0; ar < nArea; ++ar) {
          // skips if users provide values
          if (std::isnan(Distribution(sim, y, fl, ar))) {
            Distribution(sim, y, fl, ar) = UtilTheta[ar] * inv_total;  
          }
        }
      } else { 
        // No exploitable biomass in any area for this fleet (e.g. zero retention or selectivity)
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar))) {
            Distribution(sim, y, fl, ar) = 0.0;
          }
        }
      }
    }
  }
  
}

#endif // CALC_SPATIAL_DISTRIBUTION_H
 