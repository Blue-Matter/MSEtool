#ifndef CALC_NUMBER_NEXT_H
#define CALC_NUMBER_NEXT_H

#include <Rcpp.h>
#include <cmath>
#include <vector>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Calculates numbers at beginning of next time step and distribute over aresa
inline void CalcNumberNext(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array4D>& Number,
    const std::vector<Array5D>& FDeadArea,
    const std::vector<ConstArrayView3D> NaturalMortality,
    const std::vector<ConstArrayView3D> Semelparous,
    const ConstArrayView1D PlusGroup,
    const std::vector<ConstArrayView5D> Movement,
    const int nStock,
    const int nFleet,
    const int nArea) {
  
  for (int st = 0; st < nStock; ++st) {
    
    auto& Num_st = Number[st];                // sim, age, year, area
    const auto& Fd = FDeadArea[st];           // sim, age, year, fleet, area
    const auto& M_st = NaturalMortality[st];  // sim, age, year
    const auto& Sem_st = Semelparous[st];     // sim, age, year
    const auto& Mov_st = Movement[st];        // sim, from, to, age, year
    const int plusgroup_st = PlusGroup(st);   // st
  
    const int nAge = Num_st.dim[1];
    const int nYear = Num_st.dim[2];
    
    if (y +1 >= nYear) continue; 
    
    if (Mov_st.dim[1] != nArea || Mov_st.dim[2] != nArea)
      Rcpp::stop("Movement array has wrong area dimensions");
    
    for (int sim : Sims) {
      
      for (int age = 1; age < nAge; ++age) {
        for (int area = 0; area < nArea; ++area) {
          // ensure next-year numbers are 0 (except recruits)
          Num_st(sim, age, y + 1, area) = 0.0; 
        }
      }
        
      // survival and aging
      for (int area = 0; area < nArea; ++area) {
        
        for (int age=0; age<(nAge-1); ++age) {
          
          const double M = M_st(sim, age, y);
          const double Sem = Sem_st(sim, age, y);
          double F = 0;
          for (int fl=0; fl<nFleet; ++fl) {
            F += Fd(sim, age, y, fl, area);
          }
          const double Z = F + M;
          
          if (Sem < 0.0 || Sem > 1.0)
            Rcpp::stop("Semelparity outside [0,1]");
          
          Num_st(sim, age+1, y+1, area) = Num_st(sim,age,y,area) * std::exp(-Z) * (1-Sem);
        } 
        
        if (plusgroup_st) {
          const int age = nAge - 1;
          const double M   = M_st(sim, age, y);
          const double Sem = Sem_st(sim, age, y);
          double F = 0;
          for (int fl=0; fl<nFleet; ++fl) {
            F += Fd(sim, age, y, fl, area);
          }
          const double Z   = F + M;
          Num_st(sim, age, y + 1, area) += Num_st(sim, age, y, area) * std::exp(-Z) * (1 - Sem);
        }
      } // end survival and aging
      
      if (nArea > 1) {
        //  movement among areas for age class 2+ (first age class distributed at recruitment)
        std::vector<double> N_to(nArea); 
        
        for (int age=1; age<nAge; ++age) {
          
          std::fill(N_to.begin(), N_to.end(), 0.0);
          
          for (int fromArea=0; fromArea<nArea; ++fromArea) {
            const double Nfrom = Num_st(sim, age, y + 1, fromArea);
            
            if (Nfrom == 0.0) continue;
            
            double p_sum = 0.0; // mov prob sum
            
            for (int toArea = 0; toArea < nArea; ++toArea) {
              const double p = Mov_st(sim, fromArea, toArea, age, y + 1);
              N_to[toArea] += Nfrom * p;
              p_sum += p;
            }
            if (std::fabs(p_sum - 1.0) > 1e-8)
              Rcpp::stop("Movement probabilities do not sum to 1");
          }
          
          for (int toArea = 0; toArea < nArea; ++toArea) {
            Num_st(sim, age, y + 1, toArea) = N_to[toArea];
          } 
        }
      } // end movement
      
    } // end sim loop
  } // end stock loop
}
#endif