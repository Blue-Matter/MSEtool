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
    const int nSim,
    std::vector<Array5D>& LandingsAtAge,                    // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& DiscardsAtAge,
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FRetainArea,
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                // sim, age, year, area
    const int nAge = Num_st.dim[1];
    
    const auto& Fd = FDeadArea[st];           // sim, age, year, fleet, area
    const auto& Fr = FRetainArea[st];         // sim, age, year, fleet, area
    const auto& M_st = NaturalMortality[st];  // sim, age, year
    
    auto& LAA_st = LandingsAtAge[st];
    auto& DAA_st = DiscardsAtAge[st];
    
    // temp vectors for age-vectorization calcs
    std::vector<double> Z_age(nAge);
    std::vector<double> N_dead_age(nAge);
    
    for (int sim = 0; sim < nSim; ++sim) {
      for (int area = 0; area < nArea; ++area) {
    
          // Calculate  age- area- total mortality
          for (int age = 0; age < nAge; ++age) {
            double F_sum = 0.0;
            for (int fl = 0; fl < nFleet; ++fl)
              F_sum += Fd(sim, age, y, fl, area);
            
            Z_age[age] = M_st(sim, age, y) + F_sum;
            N_dead_age[age] = Num_st(sim, age, y, area) * (1.0 - std::exp(-Z_age[age]));
          }
          
          for (int fl = 0; fl < nFleet; ++fl) {
            for (int age = 0; age < nAge; ++age) {
              if (Z_age[age] > 0.0) {
                const double Fr_ratio = Fr(sim, age, y, fl, area) / Z_age[age];
                const double Fdisc_ratio = std::max(Fd(sim, age, y, fl, area) - Fr(sim, age, y, fl, area), 0.0) / Z_age[age];
                LAA_st(sim, age, y, fl, area) = Fr_ratio * N_dead_age[age];
                DAA_st(sim, age, y, fl, area) = Fdisc_ratio * N_dead_age[age];
              } else { 
                LAA_st(sim, age, y, fl, area) = 0.0;
                DAA_st(sim, age, y, fl, area) = 0.0;
              } 
            } // end age loop 
          } // end fleet loop
      } // end area loop
    } // end sim loop
  } // end stock loop
}

#endif 