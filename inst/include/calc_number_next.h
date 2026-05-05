#ifndef CALC_NUMBER_NEXT_H
#define CALC_NUMBER_NEXT_H

#include <Rcpp.h>
#include <cmath>
#include <vector>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Calculates numbers at beginning of next time step and distribute over areas
inline void CalcNumberNext(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array4D>& Number,                           // [stock] sim, age, year, area
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<ConstArrayView3D> Semelparous,        // [stock] sim, age, year
    const ConstArrayView1D PlusGroup,                       // stock
    const std::vector<ConstArrayView5D> Movement,           // [stock] sim, from, to, age, year
    const int nStock,
    const int nFleet,
    const int nArea) {
  
  // Checks
  if ((int)Number.size() < nStock ||
      (int)FDeadArea.size() < nStock ||
      (int)NaturalMortality.size() < nStock ||
      (int)Semelparous.size() < nStock ||
      (int)Movement.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  
  std::vector<double> Z_buf;
  std::vector<double> N_to(nArea);
  
  for (int st = 0; st < nStock; ++st) {
    
    auto& Num_st       = Number[st];           // sim, age, year, area
    const auto& Fd     = FDeadArea[st];        // sim, age, year, fleet, area
    const auto& M_st   = NaturalMortality[st]; // sim, age, year
    const auto& Sem_st = Semelparous[st];      // sim, age, year
    const auto& Mov_st = Movement[st];         // sim, from, to, age, year
    
    const int plusgroup_st = static_cast<int>(PlusGroup(st));
    const int nAge = Num_st.dim[1];
    const int nYear = Num_st.dim[2];
    
    if (y + 1 >= nYear) continue; 
    
    if (Mov_st.dim[1] != nArea || Mov_st.dim[2] != nArea)
      Rcpp::stop("Movement array has wrong area dimensions for stock " +
        std::to_string(st + 1));
    
    Z_buf.resize(nAge * nArea);
    
    for (int sim : Sims) {
      
      const int sim_num   = sim_index<4>(sim, Num_st, "Number");
      
      // Skip if Number[y+1] is already populated 
      // Check all age (1+) x area cells — if any are non-zero, skip this sim.
      // Age 0 is handled by CalcRecruitment so is excluded from the check.
      bool already_populated = false;
      for (int age = 1; age < nAge && !already_populated; ++age) {
        for (int ar = 0; ar < nArea && !already_populated; ++ar) {
          if (Num_st(sim_num, age, y + 1, ar) > 0.0) {
            already_populated = true;
          }
        }
      }
      if (already_populated) continue;
          
      const int sim_fd    = sim_index<5>(sim, Fd, "FDeadArea");
      const int sim_M     = sim_index<3>(sim, M_st, "NaturalMortality");
      const int sim_sem   = sim_index<3>(sim, Sem_st, "Semelparous");
      const int sim_mov   = sim_index<5>(sim, Mov_st, "Movement");
      
      // Compute Z_buf(age, area) = M(age) + sum_fl F_dead 
      for (int age = 0; age < nAge; ++age) {
        const double M = M_st(sim_M, age, y);   
        for (int ar = 0; ar < nArea; ++ar) {
          double F = 0.0;
          for (int fl = 0; fl < nFleet; ++fl)
            F += Fd(sim_fd, age, y, fl, ar);
          Z_buf[age * nArea + ar] = M + F;
        }
      } 
      
      // Reset next-year numbers (age 1+; age 0 set by CalcRecruitment)
      for (int age = 1; age < nAge; ++age)
        for (int ar = 0; ar < nArea; ++ar)
          Num_st(sim_num, age, y + 1, ar) = 0.0;
      
      // survival and aging
      for (int age = 0; age < nAge - 1; ++age) {
        const double Sem = Sem_st(sim_sem, age, y);   
        const double survive = 1.0 - Sem; 
        
        for (int ar = 0; ar < nArea; ++ar) {
          Num_st(sim_num, age + 1, y + 1, ar) = Num_st(sim_num, age, y, ar) 
          * std::exp(-Z_buf[age * nArea + ar])
          * survive;
        }
      }
      
      // Plus-group accumulation
      if (plusgroup_st) {
        const int age    = nAge - 1;
        const double Sem = Sem_st(sim_sem, age, y);
        const double survive = 1.0 - Sem;
        
        for (int ar = 0; ar < nArea; ++ar) {
          Num_st(sim_num, age, y + 1, ar) +=
            Num_st(sim_num, age, y, ar)
          * std::exp(-Z_buf[age * nArea + ar])
          * survive;
        }
      }
      
      // Movement among areas (age 1+ only; age 0 distributed at recruitment)
      if (nArea > 1) {
        
        for (int age = 1; age < nAge; ++age) {
          
          std::fill(N_to.begin(), N_to.end(), 0.0);
          
          for (int fromArea = 0; fromArea < nArea; ++fromArea) {
            const double Nfrom = Num_st(sim_num, age, y + 1, fromArea);
            if (Nfrom == 0.0) continue;
          
            double p_sum = 0.0;
            for (int toArea = 0; toArea < nArea; ++toArea) {
              const double p = Mov_st(sim_mov, fromArea, toArea, age, y + 1);
              N_to[toArea] += Nfrom * p;
              p_sum += p;
            }
            
            // TODO in R 
            // if (std::fabs(p_sum - 1.0) > 1e-8)
            //   Rcpp::stop(
            //     "Movement probabilities do not sum to 1 "
            //     "(stock=" + std::to_string(st + 1) +
            //       ", sim="  + std::to_string(sim + 1) +
            //       ", age="  + std::to_string(age + 1) +
            //       ", from=" + std::to_string(fromArea + 1) + ")"
            //   );
          }
          for (int toArea = 0; toArea < nArea; ++toArea)
            Num_st(sim_num, age, y + 1, toArea) = N_to[toArea];
        }
      } // end movement
    } // end sim loop
  } // end stock loop
} 
#endif