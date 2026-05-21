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
  
  constexpr bool InterFleetComp = true; // inter-fleet competition (hard-coded for now)
  
  
  // Single area
  if (nArea == 1) {
    for (int sim : Sims)
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }
  
  // ------------------------------------------------------------------
  const int SFA = nSim * nFleet * nArea;  // total cells per nSim × nFleet × nArea buffer
  
  static thread_local std::vector<double> B_hat_buf;
  static thread_local std::vector<double> D0_buf;
  static thread_local std::vector<double> U_buf;
  static thread_local std::vector<double> U_tilde_buf;
  static thread_local std::vector<double> weights_buf;
  

  static thread_local std::vector<double> B_stock_buf;
  static thread_local bool initialized = false;

  if (!initialized) {
    B_hat_buf.resize(SFA);
    D0_buf.resize(SFA);
    U_buf.resize(SFA);
    U_tilde_buf.resize(SFA);
    weights_buf.resize(nArea);
    B_stock_buf.resize(nStock * SFA);
    initialized = true;
  }


  auto sfa_idx = [&](int sim, int fl, int ar) -> int {
    return sim * nFleet * nArea + fl * nArea + ar;
  }; 
  auto bst_idx = [&](int st, int sim, int fl, int ar) -> int {
    return st * SFA + sim * nFleet * nArea + fl * nArea + ar;
  };

  // Zero only the active Sims
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      for (int ar = 0; ar < nArea; ++ar) {
        const int i = sfa_idx(sim, fl, ar);
        B_hat_buf[i] = 0.0;
        D0_buf[i]    = 0.0;
        U_buf[i]     = 0.0;
        U_tilde_buf[i] = 0.0;
      }
    }
  } 
  for (int st = 0; st < nStock; ++st)
    for (int sim : Sims)
      for (int fl = 0; fl < nFleet; ++fl)
        for (int ar = 0; ar < nArea; ++ar)
          B_stock_buf[bst_idx(st, sim, fl, ar)] = 0.0;


  // Stage 1 — Raw exploitable biomass B_hat(sim, fl, ar)
  //           and per-stock B_stock[st](sim, fl, ar)
  for (int st = 0; st < nStock; ++st) {
    const Array4D&          Num_st = Number[st];
    const ConstArrayView4D& Wgt_st = WeightFleet[st];
    const ConstArrayView5D& Sel_st = SelAge[st];
    const ConstArrayView5D& Ret_st = RetAge[st];
    const int nAge = Num_st.dim[1];
     
    for (int sim : Sims) {
      const int sim_num = sim_index<4>(sim, Num_st);
      const int sim_cl  = sim_index<5>(sim, Closure);
      const int sim_wgt = sim_index<4>(sim, Wgt_st);
      const int sim_sel = sim_index<5>(sim, Sel_st);
      const int sim_ret = sim_index<5>(sim, Ret_st);
      const int sim_q   = sim_index<4>(sim, q);
    
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_val = q(sim_q, st, y, fl);
        if (q_val <= 0.0) continue;
      
        for (int ar = 0; ar < nArea; ++ar) {
          if (Closure(sim_cl, st, y, fl, ar) <= 0.0) continue;
        
          double B_sfra = 0.0;
          for (int age = 0; age < nAge; ++age) {
            B_sfra +=
              Num_st(sim_num, age, y, ar)        *
              Wgt_st(sim_wgt, age, y, fl)        *
              Sel_st(sim_sel, age, y, fl, ar)    *
              Ret_st(sim_ret, age, y, fl, ar);
          } 
          const double qB = q_val * B_sfra;
          B_stock_buf[bst_idx(st, sim, fl, ar)] = qB;
          B_hat_buf[sfa_idx(sim, fl, ar)]       += qB;
        }
      }
    }
  } // end stage 1
   
  // Optionally convert B_hat to density (divide by relative area size)
  for (int sim : Sims) {
    const int sim_rs = sim_index<2>(sim, RelSize);
    for (int fl = 0; fl < nFleet; ++fl) {
      if (!UseDensity[fl]) continue;
      for (int ar = 0; ar < nArea; ++ar) {
        const double A = RelSize(sim_rs, ar);
        const int i = sfa_idx(sim, fl, ar);
        B_hat_buf[i] = (A > 0.0) ? B_hat_buf[i] / A : 0.0;
      }
    }
  }
   
  
  // First-pass IFD: D0 proportional to B_hat / density
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total += B_hat_buf[sfa_idx(sim, fl, ar)];
      if (total <= 0.0) continue;
      const double inv = 1.0 / total;
      for (int ar = 0; ar < nArea; ++ar)
        D0_buf[sfa_idx(sim, fl, ar)] = B_hat_buf[sfa_idx(sim, fl, ar)] * inv;
    }
  }
   
  // Stage 2 — Per-stock depletion-adjusted utility U(sim, fl, ar)
  for (int st = 0; st < nStock; ++st) {
    for (int sim : Sims) {
      const int sim_q  = sim_index<4>(sim, q);
      const int sim_rs = sim_index<2>(sim, RelSize);
      
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_val = q(sim_q, st, y, fl);
        if (q_val <= 0.0) continue;
      
        const double E_sf = Effort(sim, y, fl);
        
        for (int ar = 0; ar < nArea; ++ar) {
          const double B_k = B_stock_buf[bst_idx(st, sim, fl, ar)];
          if (B_k <= 0.0) continue;
          
          const double D0_ar = D0_buf[sfa_idx(sim, fl, ar)];
          
          // Own-fleet local fishing pressure
          double phi = 0.0;
          if (UseDensity[fl]) {
            const double A = RelSize(sim_rs, ar);
            phi = (A > 0.0) ? q_val * E_sf * D0_ar / A : 0.0;
          } else {
            phi = q_val * E_sf * D0_ar;
          }
          
          // Competitor-fleet pressure (one-timestep lag)
          if (nFleet > 1 && InterFleetComp && y > 0) {
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
            // Third-order Taylor expansion near zero: avoids division by very small phi
            h = 1.0 - phi * 0.5 + (phi * phi) * (1.0 / 6.0);
          }
          
          // Depletion-adjusted contribution
          double U_k = B_k * h;
          if (UseDensity[fl]) {
            const double A = RelSize(sim_rs, ar);
            U_k = (A > 0.0) ? U_k / A : 0.0;
          }
          
          U_buf[sfa_idx(sim, fl, ar)] += U_k;
        }
      }
    }
  } // end stage 2
  
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total_U = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total_U += U_buf[sfa_idx(sim, fl, ar)];
      
      if (total_U <= 0.0) {
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = 0.0;
        }
        continue; 
      }
      
      const double inv = 1.0 / total_U;
      for (int ar = 0; ar < nArea; ++ar)
        U_tilde_buf[sfa_idx(sim, fl, ar)] = U_buf[sfa_idx(sim, fl, ar)] * inv;
    }
  }
  
  // Stage 3 — Softmax spatial targeting & write Distribution
  for (int sim : Sims) {
    for (int fl = 0; fl < nFleet; ++fl) {
      
      double total_U = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total_U += U_buf[sfa_idx(sim, fl, ar)];
      if (total_U <= 0.0) continue;
      
      const int    sim_t  = sim_index<3>(sim, Spatial_Targeting);
      const double lambda = Spatial_Targeting(sim_t, y, fl);
      
      if (lambda <= 0.0) {
        // Uniform share across open areas (U > 0 ↔ area accessible)
        int n_open = 0;
        for (int ar = 0; ar < nArea; ++ar) {
          if (U_buf[sfa_idx(sim, fl, ar)] > 0.0) ++n_open;
        }
        const double share = (n_open > 0) ? 1.0 / n_open : 0.0;
        for (int ar = 0; ar < nArea; ++ar) {
          if (std::isnan(Distribution(sim, y, fl, ar))) {
            Distribution(sim, y, fl, ar) =
              (U_buf[sfa_idx(sim, fl, ar)] > 0.0) ? share : 0.0;
          }
        }
        continue;
      } 
      
      // Softmax with log-sum-exp shift for numerical stability.
      double max_u = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        max_u = std::max(max_u, U_tilde_buf[sfa_idx(sim, fl, ar)]);
      
      double sum_w = 0.0;
      for (int ar = 0; ar < nArea; ++ar) {
        const double u = U_tilde_buf[sfa_idx(sim, fl, ar)];
        if (u > 0.0) {
          const double w = std::exp(lambda * (u - max_u));
          weights_buf[ar] = w;
          sum_w += w;
        } else {
          weights_buf[ar] = 0.0;
        }
      }
      
      if (sum_w > 0.0) {
        const double inv_sum = 1.0 / sum_w;
        for (int ar = 0; ar < nArea; ++ar)
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = weights_buf[ar] * inv_sum;
      } else {
        for (int ar = 0; ar < nArea; ++ar)
          if (std::isnan(Distribution(sim, y, fl, ar)))
            Distribution(sim, y, fl, ar) = 0.0;
      }
    }
  } // end stage 3
}

#endif // CALC_SPATIAL_DISTRIBUTION_H