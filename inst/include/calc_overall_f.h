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
    const std::vector<Array5D>& FInteractArea,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FDeadArea,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FRetainArea,              // [stock] sim, age, year, fleet, area
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  static constexpr double eps = 1e-12;
  
  std::vector<double> apical_interact(nFleet);
  std::vector<double> apical_dead(nFleet);
  std::vector<double> apical_retain(nFleet);
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];          // sim, age, year, area
    const auto& FIA_st = FInteractArea[st];   // sim, age, year, fleet, area
    const auto& FDA_st = FDeadArea[st];       // sim, age, year, fleet, area
    const auto& FRA_st = FRetainArea[st];     // sim, age, year, fleet, area
    
    int nAge = Num_st.dim[1];
    
    for (int sim : Sims) {
      
      std::fill(apical_interact.begin(), apical_interact.end(), 0.0);
      std::fill(apical_dead.begin(),     apical_dead.end(),     0.0);
      std::fill(apical_retain.begin(),   apical_retain.end(),   0.0);
      
      for (int age = 0; age < nAge; ++age) {
        
        // total N at this age across areas (for weighting)
        double N_age_total = 0.0;
        for (int ar = 0; ar < nArea; ++ar)
          N_age_total += Num_st(sim, age, y, ar);
        N_age_total = std::max(N_age_total, eps);
        const double inv_N_age = 1.0 / N_age_total;
         
        for (int fl = 0; fl < nFleet; ++fl) {
           
          // area-weighted average F-at-age for this fleet
          double F_interact_age = 0.0;
          double F_dead_age     = 0.0;
          double F_retain_age   = 0.0;
          
          for (int ar = 0; ar < nArea; ++ar) {
            const double w = Num_st(sim, age, y, ar) * inv_N_age;
            F_interact_age += w * FIA_st(sim, age, y, fl, ar);
            F_dead_age     += w * FDA_st(sim, age, y, fl, ar);
            F_retain_age   += w * FRA_st(sim, age, y, fl, ar);
          }
          
          // apical (max over ages)
          apical_interact[fl] = std::max(apical_interact[fl], F_interact_age);
          apical_dead[fl]     = std::max(apical_dead[fl],     F_dead_age);
          apical_retain[fl]   = std::max(apical_retain[fl],   F_retain_age);
        }
      } 
      
      for (int fl = 0; fl < nFleet; ++fl) {
        FInteract(sim, st, y, fl) = apical_interact[fl];
        FDead    (sim, st, y, fl) = apical_dead[fl];
        FRetain  (sim, st, y, fl) = apical_retain[fl];
      }
    } // end sim loop
  }   // end stock loop
} 
#endif
 