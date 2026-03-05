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
    std::vector<Array5D>& InteractAtAge,                    // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& LandingsAtAge,                    // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& DiscardsAtAge,                    // [stock] sim, age, year, fleet, area
    Array4D& Interactions,                                  // sim, stock, year, fleet
    Array4D& Landings,                                      // sim, stock, year, fleet
    Array4D& Discards,                                      // sim, stock, year, fleet
    const std::vector<Array5D>& FInteractArea,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FRetainArea,                // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& FleetWeight,       // [stock] sim, age, year, fleet
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  std::vector<double> Z_buf;          // Z(age, area)       = M + sum_fl Fd
  std::vector<double> Ndead_buf;      // N_dead(age, area)  = N * (1 - exp(-Z))
  std::vector<double> Fi_ratio_buf;   // (age, fleet, area)
  std::vector<double> Fr_ratio_buf;   // (age, fleet, area)
  std::vector<double> Fd_ratio_buf;   // discard dead ratio (age, fleet, area)
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                // sim, age, year, area
    const auto& Fi = FInteractArea[st];                 // sim, age, year, fleet, area
    const auto& Fd = FDeadArea[st];                 // sim, age, year, fleet, area
    const auto& Fr = FRetainArea[st];               // sim, age, year, fleet, area
    const auto& M_st = NaturalMortality[st];        // sim, age, year
    const auto& FWght_st = FleetWeight[st];         // sim, age, fleet, year 
    
    auto& IAA_st = InteractAtAge[st];
    auto& LAA_st = LandingsAtAge[st];
    auto& DAA_st = DiscardsAtAge[st];
    
    const int nAge = Num_st.dim[1];
    
    const int nAgeArea      = nAge * nArea;
    const int nAgeFleetArea = nAge * nFleet * nArea;
    Z_buf.resize(nAgeArea);
    Ndead_buf.resize(nAgeArea);
    Fi_ratio_buf.resize(nAgeFleetArea);
    Fr_ratio_buf.resize(nAgeFleetArea);
    Fd_ratio_buf.resize(nAgeFleetArea);
    
    for (int sim : Sims) {
      
      // temp vectors for age-vectorization calcs
      std::vector<double> Z_age(nAge);
      std::vector<double> N_dead_age(nAge);
      
      const int sim_num   = sim_index<4>(sim, Num_st,  "Number");
      const int sim_fi    = sim_index<5>(sim, Fi,      "FInteractArea");
      const int sim_fd    = sim_index<5>(sim, Fd,      "FDeadArea");
      const int sim_fr    = sim_index<5>(sim, Fr,      "FRetainArea");
      const int sim_M     = sim_index<3>(sim, M_st,    "NaturalMortality");
      const int sim_iaa   = sim_index<5>(sim, IAA_st,  "InteractAtAge");
      const int sim_laa   = sim_index<5>(sim, LAA_st,  "LandingsAtAge");
      const int sim_daa   = sim_index<5>(sim, DAA_st,  "DiscardsAtAge");
      const int sim_fw    = sim_index<4>(sim, FWght_st,"FleetWeight");
      
      for (int fl = 0; fl < nFleet; ++fl) {
        Interactions(sim, st, y, fl) = 0.0;
        Landings(sim, st, y, fl) = 0.0;
        Discards(sim, st, y, fl) = 0.0;
      }
      
      // Compute Z and NDead
      for (int age = 0; age < nAge; ++age) {
        const double M = M_st(sim_M, age, y);   
        for (int ar = 0; ar < nArea; ++ar) {
          double F_sum = 0.0;
          for (int fl = 0; fl < nFleet; ++fl)
            F_sum += Fd(sim_fd, age, y, fl, ar);
          const double Z = M + F_sum;
          const int idx  = age * nArea + ar;
          Z_buf[idx]     = Z;
          Ndead_buf[idx] = Num_st(sim_num, age, y, ar) * (1.0 - std::exp(-Z));
        }
      } 
      
      // Compute F-ratio buffers
      for (int age = 0; age < nAge; ++age) {
        for (int fl = 0; fl < nFleet; ++fl) {
          for (int ar = 0; ar < nArea; ++ar) {
            const int bidx = age * nFleet * nArea + fl * nArea + ar;
            const double Z = Z_buf[age * nArea + ar];
            if (Z > 0.0) {
              const double inv_Z   = 1.0 / Z;
              const double fi_val  = Fi(sim_fi, age, y, fl, ar);
              const double fr_val  = Fr(sim_fr, age, y, fl, ar);
              const double fdisc   = std::max(Fd(sim_fd, age, y, fl, ar) - fr_val, 0.0);
              Fi_ratio_buf[bidx] = fi_val  * inv_Z;
              Fr_ratio_buf[bidx] = fr_val  * inv_Z;
              Fd_ratio_buf[bidx] = fdisc   * inv_Z;
            } else {
              Fi_ratio_buf[bidx] = 0.0;
              Fr_ratio_buf[bidx] = 0.0;
              Fd_ratio_buf[bidx] = 0.0;
            }
          }
        }
      }
      
      // Calc Catch biomass
      for (int fl = 0; fl < nFleet; ++fl) {
        for (int age = 0; age < nAge; ++age) {
          const double W = FWght_st(sim_fw, age, y, fl);
          
          for (int ar = 0; ar < nArea; ++ar) {
            const double Ndead = Ndead_buf[age * nArea + ar];
            const int    bidx  = age * nFleet * nArea + fl * nArea + ar;
            
            const double Inum = Fi_ratio_buf[bidx] * Ndead;
            const double Lnum = Fr_ratio_buf[bidx] * Ndead;
            const double Dnum = Fd_ratio_buf[bidx] * Ndead;
             
            IAA_st(sim_iaa, age, y, fl, ar) = Inum;
            LAA_st(sim_laa, age, y, fl, ar) = Lnum;
            DAA_st(sim_daa, age, y, fl, ar) = Dnum;
          
            const double IW = Inum * W;
            const double LW = Lnum * W;
            const double DW = Dnum * W;
          
            Interactions(sim, st, y, fl) += IW;
            Landings(sim, st, y, fl)     += LW;
            Discards(sim, st, y, fl)     += DW;
          }
        }
      }
      
    } // end sim loop
  }   // end stock loop
}
#endif 