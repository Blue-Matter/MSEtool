#ifndef CALC_OVERALLF_H
#define CALC_OVERALLF_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcOverallF(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    Array4D& FInteract,                                     // sim, stock, year, fleet
    Array4D& FDead,                                         // sim, stock, year, fleet
    Array4D& FRetain,                                       // sim, stock, year, fleet
    const std::vector<Array5D>& InteractAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& LandingsAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& DiscardsAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  static constexpr double eps = 1e-12;
  static constexpr double one_minus_eps = 1.0 - eps;
  
  std::vector<double> Interact_buf;  // fleet
  std::vector<double> Land_buf;      // fleet
  std::vector<double> DeadDisc_buf;  // fleet
  
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                  // sim, age, year, area
    const auto& IAA_st = InteractAtAge[st];           // sim, age, year, fleet, area
    const auto& LAA_st = LandingsAtAge[st];           // sim, age, year, fleet, area
    const auto& DAA_st = DiscardsAtAge[st];           // sim, age, year, fleet, area
    
    int nAge = Num_st.dim[1];
    
    Interact_buf.resize(nFleet);
    Land_buf.resize(nFleet);
    DeadDisc_buf.resize(nFleet);

    for (int sim : Sims) {
      
      double N_total = 0.0;
      std::fill(Interact_buf.begin(), Interact_buf.end(), 0.0);
      std::fill(Land_buf.begin(),     Land_buf.end(),     0.0);
      std::fill(DeadDisc_buf.begin(), DeadDisc_buf.end(), 0.0);
      
      for (int age = 0; age < nAge; ++age) {
        for (int ar = 0; ar < nArea; ++ar) {
          
          N_total += Num_st(sim, age, y, ar);
          
          for (int fl = 0; fl < nFleet; ++fl) {
            Interact_buf[fl] += IAA_st(sim, age, y, fl, ar);
            Land_buf[fl]     += LAA_st(sim, age, y, fl, ar);
            DeadDisc_buf[fl] += DAA_st(sim, age, y, fl, ar);
          }
        }
      }     
      
      N_total = std::max(N_total, eps);
      const double inv_N = 1.0 / N_total;
      
      for (int fl = 0; fl < nFleet; ++fl) {
        
        const double TotalDead = Land_buf[fl] + DeadDisc_buf[fl];
        
        const double ratio_interact = std::min(Interact_buf[fl] * inv_N, one_minus_eps);
        const double ratio_dead     = std::min(TotalDead        * inv_N, one_minus_eps);
        const double ratio_retain   = std::min(Land_buf[fl]     * inv_N, one_minus_eps);
         
        FInteract(sim, st, y, fl) = -std::log(1.0 - ratio_interact);
        FDead(sim,     st, y, fl) = -std::log(1.0 - ratio_dead);
        FRetain(sim,   st, y, fl) = -std::log(1.0 - ratio_retain);
      }
      
    } // end sim loop
  }   // end stock loop
} 
#endif
 