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
  
  const double eps = 1e-12;
  
  for (int st = 0; st < nStock; ++st) {
    
    
    const auto& Num_st = Number[st];                  // sim, age, year, area
    const auto& IAA_st = InteractAtAge[st];           // sim, age, year, fleet, area
    const auto& LAA_st = LandingsAtAge[st];           // sim, age, year, fleet, area
    const auto& DAA_st = DiscardsAtAge[st];           // sim, age, year, fleet, area
    
    int nAge = Num_st.dim[1];
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    check_dims<5>(IAA_st, {nSim, nAge, IAA_st.dim[2], nFleet, nArea}, "InteractAtAge", y, 2);
    check_dims<5>(LAA_st, {nSim, nAge, LAA_st.dim[2], nFleet, nArea}, "LandingsAtAge", y, 2);
    check_dims<5>(DAA_st, {nSim, nAge, DAA_st.dim[2], nFleet, nArea}, "DiscardsAtAge", y, 2);
    
    for (int sim : Sims) {
      
      // sum N over ages and areas
      double N_total = 0.0;
      for (int age = 0; age < nAge; ++age) {
        for (int area = 0; area < nArea; ++area) {
          N_total += Num_st(sim, age, y, area);  
        }  
      }
      N_total = std::max(N_total, eps);
      
      //  sum landings and discards over ages and areas 
      for (int fl = 0; fl < nFleet; ++fl) {
        double Interact_total  = 0.0;
        double Land_total  = 0.0;
        double DeadDisc_total  = 0.0;
        
        for (int age = 0; age < nAge; ++age) {
          for (int area = 0; area < nArea; ++area) {
            double inter_val = IAA_st(sim, age, y, fl, area);
            double land_val = LAA_st(sim, age, y, fl, area);
            double dead_val = DAA_st(sim, age, y, fl, area);
            
            Interact_total += inter_val;
            Land_total += land_val;
            DeadDisc_total += dead_val;
        
          }
        }
        
        const double TotalDead = Land_total + DeadDisc_total;
        
        // Instantaneous overall F 
        double ratio_dead   = std::min(TotalDead / N_total, 1.0 - eps);
        double ratio_retain = std::min(Land_total / N_total, 1.0 - eps);
        double ratio_interact = std::min(Interact_total / N_total, 1.0 - eps);
        
        FInteract(sim, st, y, fl) = -std::log(1.0 - ratio_interact);
        FDead(sim,   st, y, fl) = -std::log(1.0 - ratio_dead);
        FRetain(sim, st, y, fl) = -std::log(1.0 - ratio_retain);
    
      } // end fleet loop 
      
    } // end sim loop
  }  // end stock loop
}

#endif
 