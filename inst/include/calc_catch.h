#ifndef CALC_CATCH_H
#define CALC_CATCH_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Calculates catch-at-age - 
// catch-at-size handled elsewhere - full ALK is very large. need broadcasting over sim / year or calculate internally 

inline void CalcCatch(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array5D>& LandingsAtAge,                    // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& DiscardsAtAge,                    // [stock] sim, age, year, fleet, area
    Array4D& Landings,                                      // sim, stock, year, fleet
    Array4D& Discards,                                      // sim, stock, year, fleet
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FRetainArea,                // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& FleetWeight,       // [stock] sim, age, year, fleet
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  // Checks 
  if ((int)LandingsAtAge.size() < nStock ||
      (int)DiscardsAtAge.size() < nStock ||
      (int)FDeadArea.size() < nStock ||
      (int)FRetainArea.size() < nStock ||
      (int)NaturalMortality.size() < nStock ||
      (int)Number.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                // sim, age, year, area
    const auto& Fd = FDeadArea[st];                 // sim, age, year, fleet, area
    const auto& Fr = FRetainArea[st];               // sim, age, year, fleet, area
    const auto& M_st = NaturalMortality[st];        // sim, age, year
    const auto& FWght_st = FleetWeight[st];         // sim, age, fleet, year 
    
    const int nAge = Num_st.dim[1];
    auto& LAA_st = LandingsAtAge[st];
    auto& DAA_st = DiscardsAtAge[st];
    
    check_dims<5>(Fd, {nSim, nAge, Fd.dim[2], nFleet, nArea}, "FDeadArea", y, 2);
    check_dims<5>(Fr, {nSim, nAge, Fr.dim[2], nFleet, nArea}, "FRetainArea", y, 2);
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    check_dims<5>(LAA_st, {nSim, nAge, LAA_st.dim[2], nFleet, nArea}, "LandingsAtAge", y, 2);
    check_dims<5>(DAA_st, {nSim, nAge, DAA_st.dim[2], nFleet, nArea}, "DiscardsAtAge", y, 2);
    check_dims<3>(M_st, {nSim, nAge, M_st.dim[2]}, "NaturalMortality", y, 2);
    check_dims<4>(FWght_st, {nSim, nAge, FWght_st.dim[2], nFleet}, "FleetWeight", y, 2);
    
    // temp vectors for age-vectorization calcs
    std::vector<double> Z_age(nAge);
    std::vector<double> N_dead_age(nAge);
    
    for (int sim : Sims) {
      
      const int sim_num     = sim_index<4>(sim, Num_st, "Number");
      const int sim_fd      = sim_index<5>(sim, Fd, "FDeadArea");
      const int sim_fr      = sim_index<5>(sim, Fr, "FRetainArea");
      const int sim_M       = sim_index<3>(sim, M_st, "NaturalMortality");
      const int sim_laa     = sim_index<5>(sim, LAA_st, "LandingsAtAge");
      const int sim_daa     = sim_index<5>(sim, DAA_st, "DiscardsAtAge");
      const int sim_FWght   = sim_index<4>(sim, FWght_st, "FleetWeight");
      
      for (int fl = 0; fl < nFleet; ++fl) {
        Landings(sim, st, y, fl) = 0.0;
        Discards(sim, st, y, fl) = 0.0;
      }
      
      for (int area = 0; area < nArea; ++area) {
    
          // Calculate  age- area- total mortality
          for (int age = 0; age < nAge; ++age) {
            double F_sum = 0.0;
            for (int fl = 0; fl < nFleet; ++fl)
              F_sum += Fd(sim_fd, age, y, fl, area);
            
            Z_age[age] = M_st(sim_M, age, y) + F_sum;
            N_dead_age[age] = Num_st(sim_num, age, y, area) * (1.0 - std::exp(-Z_age[age]));
          }
          
          for (int fl = 0; fl < nFleet; ++fl) {
            for (int age = 0; age < nAge; ++age) {
              
              double Lnum = 0.0;
              double Dnum = 0.0;
              
              if (Z_age[age] > 0.0) {
                const double Fr_ratio = Fr(sim_fr, age, y, fl, area) / Z_age[age];
                const double Fdisc_ratio =  std::max(Fd(sim_fd, age, y, fl, area) - Fr(sim_fr, age, y, fl, area), 0.0)/ Z_age[age];
                Lnum = Fr_ratio * N_dead_age[age];
                Dnum = Fdisc_ratio * N_dead_age[age];
              } 
              
              // Store numbers
              LAA_st(sim_laa, age, y, fl, area) = Lnum;
              DAA_st(sim_daa, age, y, fl, area) = Dnum;
              
              // Convert to biomass and accumulate
              const double weight = FWght_st(sim_FWght, age, y, fl);
              
              Landings(sim, st, y, fl) += Lnum * weight;
              Discards(sim, st, y, fl) += Dnum * weight;
              
            } // end age loop 
          } // end fleet loop
      } // end area loop
      
      
    } // end sim loop
  } // end stock loop
}

#endif 