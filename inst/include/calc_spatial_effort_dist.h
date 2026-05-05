#ifndef CALC_SPATIAL_DISTRIBUTION_H
#define CALC_SPATIAL_DISTRIBUTION_H

#include <Rcpp.h>
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcSpatialDistribution(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    Array4D& Distribution,                                // sim, year, fleet, area
    const std::vector<Array4D>& Number,                   // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& WeightFleet,     // [stock] sim, age, year, fleet
    const std::vector<ConstArrayView5D>& SelAge,          // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& RetAge,          // [stock] sim, age, year, fleet, area
    const ConstArrayView4D& q,                            // sim, stock, year, fleet
    const ConstArrayView5D& Closure,                      // sim, stock, year, fleet, area
    const ConstArrayView3D& Spatial_Targeting,            // sim, year, fleet  (lambda >= 0)
    const Array3D& Effort,                                // sim, year, fleet
    const ConstArrayView2D& RelSize,                      // sim, area  (sums to 1)
    const std::vector<bool>& UseDensity,                  // [fleet]
    const int nStock,
    const int nFleet,
    const int nArea) {
  
  bool InterFleetComp = 1; // inter-fleet competition flag - currently hard-coded
  
  // Checks - TODO - remove once Hist@Misc checks complete 
  if ((int)Number.size()      < nStock ||
      (int)WeightFleet.size() < nStock ||
      (int)SelAge.size()      < nStock ||
      (int)RetAge.size()      < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  if ((int)UseDensity.size() < nFleet)
    Rcpp::stop("UseDensity must have length >= nFleet");
  
  check_dims<4>(Distribution,     {nSim, Distribution.dim[1],      nFleet, nArea}, "Distribution",     y, 1);
  check_dims<4>(q,                 {nSim, nStock, q.dim[2],         nFleet},        "q",                y, 2);
  check_dims<5>(Closure,           {nSim, nStock, Closure.dim[2],   nFleet, nArea}, "Closure",          y, 2);
  check_dims<3>(Spatial_Targeting, {nSim, Spatial_Targeting.dim[1], nFleet},        "Spatial_Targeting",y, 1);
  check_dims<3>(Effort,            {nSim, Effort.dim[1],            nFleet},        "Effort",           y, 1);
  check_dims<2>(RelSize,           {nSim, nArea},                                   "RelSize");
  
  // Single-area
  if (nArea == 1) {
    for (int sim : Sims)
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }
  
  const std::array<int, 3> dim3 = {nSim, nFleet, nArea};
  Array3D B_hat(dim3, 0.0);
  std::vector<Array3D> B_stock(nStock, Array3D(dim3, 0.0));
  
  // Stage 1 - Raw Exploitable Biomass
  for (int st = 0; st < nStock; ++st) {
    const Array4D&          Num_st = Number[st];
    const ConstArrayView4D& Wgt_st = WeightFleet[st];
    const ConstArrayView5D& Sel_st = SelAge[st];
    const ConstArrayView5D& Ret_st = RetAge[st];
    const int nAge = Num_st.dim[1];
    
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea},         "Number",      y, 2);
    check_dims<4>(Wgt_st, {nSim, nAge, Wgt_st.dim[2], nFleet},        "WeightFleet", y, 2);
    check_dims<5>(Sel_st, {nSim, nAge, Sel_st.dim[2], nFleet, nArea}, "SelAge",      y, 2);
    check_dims<5>(Ret_st, {nSim, nAge, Ret_st.dim[2], nFleet, nArea}, "RetAge",      y, 2);
    
    for (int sim : Sims) {
      const int sim_cl  = sim_index<5>(sim, Closure, "Closure");
      const int sim_wgt = sim_index<4>(sim, Wgt_st,  "Wgt_st");
      const int sim_sel = sim_index<5>(sim, Sel_st,  "Sel_st");
      const int sim_ret = sim_index<5>(sim, Ret_st,  "Ret_st");
      const int sim_q   = sim_index<4>(sim, q,       "q");
    
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_val = q(sim_q, st, y, fl);
        if (q_val <= 0.0) continue;
      
        for (int ar = 0; ar < nArea; ++ar) {
          if (Closure(sim_cl, st, y, fl, ar) <= 0.0) continue;
         
          double B_sfra = 0.0;
          for (int age = 0; age < nAge; ++age) {
            B_sfra +=
              Num_st(sim, age, y, ar)          *
              Wgt_st(sim_wgt, age, y, fl)      *
              Sel_st(sim_sel, age, y, fl, ar)  *
              Ret_st(sim_ret, age, y, fl, ar);
          } 
          const double qB = q_val * B_sfra;
          B_stock[st](sim, fl, ar) = qB;   // store per-stock for pass 2
          B_hat(sim, fl, ar)      += qB;   // accumulate total
        }
      }
    }
  } // end stage 1 stock loop 
  
  // Optionally convert total B_hat to density
  for (int sim : Sims) {
    const int sim_rs = sim_index<2>(sim, RelSize, "RelSize");
    for (int fl = 0; fl < nFleet; ++fl) {
      if (!UseDensity[fl]) continue;
      for (int ar = 0; ar < nArea; ++ar) {
        const double A = RelSize(sim_rs, ar);
        B_hat(sim, fl, ar) = (A > 0.0) ? B_hat(sim, fl, ar) / A : 0.0;
      }
    }
  }
   
  // First-pass IFD allocation: D^(0) proportional to total B_hat/density.
  Array3D D0(dim3, 0.0);
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total += B_hat(sim, fl, ar);
      if (total <= 0.0) continue;
      const double inv = 1.0 / total;
      for (int ar = 0; ar < nArea; ++ar)
        D0(sim, fl, ar) = B_hat(sim, fl, ar) * inv;
    }
  } 
  
  // Stage 2 - Per-stock depletion-adjusted utility
  Array3D U(dim3, 0.0);
  
  for (int st = 0; st < nStock; ++st) {
    
    for (int sim : Sims) {
      const int sim_q  = sim_index<4>(sim, q,  "q");
      const int sim_rs = sim_index<2>(sim, RelSize, "RelSize");
      
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_val = q(sim_q, st, y, fl);
        if (q_val <= 0.0) continue;
        
        const double E_sf = Effort(sim, y, fl);
        
        for (int ar = 0; ar < nArea; ++ar) {
          const double B_k = B_stock[st](sim, fl, ar);
          if (B_k <= 0.0) continue;
          
          const double D0_ar = D0(sim, fl, ar);
          
          // Per-stock local fishing pressure 
          double phi = 0.0;
          if (UseDensity[fl]) {
            const double A = RelSize(sim_rs, ar);
            phi = (A > 0.0) ? q_val * E_sf * D0_ar / A : 0.0;
          } else { 
            phi = q_val * E_sf * D0_ar;
          }
          
          // Competitor fleet pressure (one-timestep lag).
          if (InterFleetComp && y > 0) {
            for (int fl2 = 0; fl2 < nFleet; ++fl2) {
              if (fl2 == fl) continue;
              const double E_sf2   = Effort(sim, y, fl2);
              const double D_prior = Distribution(sim, y - 1, fl2, ar);
              if (E_sf2 <= 0.0 || std::isnan(D_prior) || D_prior <= 0.0) continue;
              
              const double q_val2 = q(sim_q, st, y, fl2);
              if (q_val2 <= 0.0) continue;
              
              if (UseDensity[fl]) {
                const double A = RelSize(sim_rs, ar);
                phi += (A > 0.0) ? q_val2 * E_sf2 * D_prior / A : 0.0;
              } else { 
                phi += q_val2 * E_sf2 * D_prior;
              }
            }
          } 
          
          // Depletion discount h(phi) = (1 - exp(-phi)) / phi
          double h = 1.0;
          if (phi > 1e-6) {
            h = (1.0 - std::exp(-phi)) / phi;
          } else if (phi > 0.0) { 
            // Third-order Taylor near zero avoids division by very small phi
            h = 1.0 - phi * 0.5 + (phi * phi) / 6.0;
          } 
          
          // Depletion-adjusted stock contribution.
          double U_k = B_k * h;
          if (UseDensity[fl]) {
            const double A = RelSize(sim_rs, ar);
            U_k = (A > 0.0) ? U_k / A : 0.0;
          } 
          
          U(sim, fl, ar) += U_k; 
        }
      }
    }
  } // end stage 2 stock loop
  
  // Normalise depletion-adjusted utility to IFD shares
  Array3D U_tilde(dim3, 0.0);
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total_U = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total_U += U(sim, fl, ar);
      if (total_U <= 0.0) continue;
      const double inv = 1.0 / total_U;
      for (int ar = 0; ar < nArea; ++ar)
        U_tilde(sim, fl, ar) = U(sim, fl, ar) * inv;
    }
  }
  
  // Stage 3 - Softmax spatial targeting
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      
      double total_U = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total_U += U(sim, fl, ar);
      
      if (total_U <= 0.0) {
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar))) {
            Distribution(sim, y, fl, ar) = 0.0;
          }
        }
        continue;
      }
      
      const int    sim_t  = sim_index<3>(sim, Spatial_Targeting, "Spatial_Targeting");
      const double lambda = Spatial_Targeting(sim_t, y, fl);
      
      if (lambda <= 0.0) {
        // Uniform across open areas
        int n_open = 0;
        
        for (int ar = 0; ar < nArea; ++ar) {
          if (U(sim, fl, ar) > 0.0) {
            ++n_open;
          }
        }
        const double share = (n_open > 0) ? 1.0 / n_open : 0.0;
        
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar))) {
            Distribution(sim, y, fl, ar) =
              (U(sim, fl, ar) > 0.0) ? share : 0.0;
          }
        }
        
        continue;
      }
      

      
      // Log-sum-exp for numerical stability
      double max_u = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        max_u = std::max(max_u, U_tilde(sim, fl, ar));
      
      std::vector<double> weights(nArea, 0.0);
      double sum_w = 0.0;
      for (int ar = 0; ar < nArea; ++ar) {
        if (U_tilde(sim, fl, ar) > 0.0) {
          const double w = std::exp(lambda * (U_tilde(sim, fl, ar) - max_u));
          weights[ar] = w;
          sum_w += w;
        }
      }
      
      if (sum_w > 0.0) {
        const double inv_sum = 1.0 / sum_w;
        for (int ar = 0; ar < nArea; ++ar)
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = weights[ar] * inv_sum;
      } else {
        for (int ar = 0; ar < nArea; ++ar)
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = 0.0;
      }
    }
  }
  
}

#endif // CALC_SPATIAL_DISTRIBUTION_H