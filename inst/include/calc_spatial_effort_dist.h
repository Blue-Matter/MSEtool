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

/**
 * Calculate spatial effort distribution across areas
 */
inline void CalcSpatialDistribution(
    const int y,                            
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
  
  // Calculate nSim - maximum number of simulations 
  const int nSim = infer_nSim(
    Distribution,
    Number,
    WeightFleet,
    SelAge,
    RetAge,
    Effort,
    q,
    RelSize
  );
  
  const std::array<int,3> dim = {nSim, nFleet, nArea};
  
  if (nArea == 1) {
    for (int sim = 0; sim < nSim; ++sim)
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }
  
  // Util array: sim, fleet, area 
  // Stored because we may want to keep this object later
  Array3D Util(dim, 0.0);
  
  // Loop over stocks
  for (int st = 0; st < nStock; ++st) { 
    Array3D B_hat(dim, 0.0);
    
    const Array4D& Num_st = Number[st];
    const ConstArrayView4D& Wgt_st = WeightFleet[st];
    const ConstArrayView5D& Sel_st = SelAge[st];
    const ConstArrayView5D& Ret_st = RetAge[st];
    
    const int nAge = Num_st.dim[1];
    
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
          
          if (A > 0.0 && B > 0.0) {
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
          const double ut = std::pow(u, theta);
          UtilTheta[ar] = ut;
          total += ut;
        } else { 
          UtilTheta[ar] = 0.0;
        }
      }
      
      if (total > 0.0) {
        const double inv_total = 1.0 / total;
        for (int ar = 0; ar < nArea; ++ar) {
          if (Distribution(sim, y, fl, ar) <= 1E-6) {
            Distribution(sim, y, fl, ar) = 0.0;
            Distribution(sim, y, fl, ar) = UtilTheta[ar] * inv_total;  
          }
          
        }
      }  
    }
  }
  
}

#endif // CALC_SPATIAL_DISTRIBUTION_H