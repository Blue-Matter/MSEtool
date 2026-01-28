#ifndef CALC_OVERALLF_H
#define CALC_OVERALLF_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"
#include "solve_F.h"

inline void CalcOverallF(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array4D>& FDead,                            // sim, age, year, fleet
    std::vector<Array4D>& FRetain,
    
    const std::vector<Array5D>& FDeadArea,
    const std::vector<Array5D>& FRetainArea,
    const std::vector<Array5D>& LandingsAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& DiscardsAtAge,
    
    const std::vector<ConstArrayView5D>& SelAge,
    const std::vector<ConstArrayView5D>& RetAge,
    const std::vector<ConstArrayView5D>& DiscMort,
    
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    
    const std::vector<ConstArrayView4D>& FleetWeight,           // [stock] sim, age, year, fleet
    
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                // sim, age, year, area
    const int nAge = Num_st.dim[1];
    const auto& Fdead_area = FDeadArea[st];           // sim, age, year, fleet, area
    const auto& Fret_area = FRetainArea[st];         // sim, age, year, fleet, area
    
    const auto& LAA_st = LandingsAtAge[st];
    const auto& DAA_st = DiscardsAtAge[st];
    
    const auto& M_st = NaturalMortality[st];  // sim, age, year
    const auto& FWght_st = FleetWeight[st];         // [stock] sim, age, fleet, year 
    
    
    auto& Fdead = FDead[st];           // sim, age, year, fleet
    auto& Fret = FRetain[st];         // sim, age, year, fleet
    
    // one area 
    if (nArea==1) {
      for (int sim = 0; sim < nSim; ++sim) {
        for (int fl = 0; fl < nFleet; ++fl) {
          for (int age = 0; age < nAge; ++age) {
            Fdead(sim, age, y, fl) = Fdead_area(sim,age, y, fl, 0);
            Fret(sim, age, y, fl) = Fret_area(sim,age, y, fl, 0);
            
          }
        }
      }
      continue; 
    }
    
    // multiple areas -> aggregate and solve for overall F
    std::vector<double> Nage_total(nAge, 0.0);
    std::vector<double> Removals_total(nFleet, 0.0);
    
    
    for (int sim : Sims) {
      // sum numbers across areas
      for (int age = 0; age < nAge; ++age) {
        Nage_total[age] = 0.0;
        for (int area = 0; area < nArea; ++area)
          Nage_total[age] += Num_st(sim, age, y, area);
      }
      
      // sim-specific FleetWeight and NaturalMortality
      std::vector<double> FWght_sim(nAge*nFleet);
      for(int age=0; age<nAge; ++age){
        for(int fl=0; fl<nFleet; ++fl){
          FWght_sim[age*nFleet + fl] = FWght_st(sim, age, y, fl);
        }
      } 
      
      std::vector<double> M_sim(nAge);
      for(int age=0; age<nAge; ++age){
        M_sim[age] = M_st(sim, age, y);
      } 
      
      // sum landings+discards across areas to get target removals (biomass/weight)
      for (int fl = 0; fl < nFleet; ++fl) {
        double totalRem = 0.0;
        for (int age = 0; age < nAge; ++age) {
          double weight = FWght_sim[age * nFleet + fl];
          double numRem = 0.0;
          for (int area = 0; area < nArea; ++area) {
            numRem += LAA_st(sim, age, y, fl, area) + DAA_st(sim, age, y, fl, area);
          }
          totalRem += numRem * weight;
        }
        Removals_total[fl] = totalRem;
      }
      
      // stock-specific weighted  selectivity / retention / discard mortality matrices (age x fleet)
      std::vector<std::vector<double>> SelMat(nAge, std::vector<double>(nFleet,0.0));
      std::vector<std::vector<double>> RetMat(nAge, std::vector<double>(nFleet,0.0));
      std::vector<std::vector<double>> DiscMat(nAge, std::vector<double>(nFleet,0.0));
      
      for (int age = 0; age < nAge; ++age) {
        for (int fl = 0; fl < nFleet; ++fl) {
          double selSum = 0.0, retSum = 0.0, discSum = 0.0;
          double FdeadSum = 0.0;
          for (int area = 0; area < nArea; ++area) {
            double F_dead_area = FDeadArea[st](sim, age, y, fl, area);
            selSum  += F_dead_area * SelAge[st](sim, age, y, fl, area);
            retSum  += F_dead_area * RetAge[st](sim, age, y, fl, area);
            discSum += F_dead_area * DiscMort[st](sim, age, y, fl, area);
            FdeadSum += F_dead_area;
          }
          if(FdeadSum > 0.0){
            SelMat[age][fl] = selSum / FdeadSum;
            RetMat[age][fl] = retSum / FdeadSum;
            DiscMat[age][fl] = discSum / FdeadSum;
          } else {  
            SelMat[age][fl] = RetMat[age][fl] = DiscMat[age][fl] = 0.0;
          }
        }
      } 
      
      SolveFResult res = SolveForF(Nage_total,        // age (number)
                                   Removals_total,    // fleet (weight)
                                   SelMat,            // age, fleet
                                   RetMat,            // age, fleet
                                   DiscMat,           // age, fleet
                                   FWght_sim,         // age, fleet
                                   M_sim);              // age
      
      // write back results
      for(int age = 0; age < nAge; ++age){
        for(int fl = 0; fl < nFleet; ++fl){
          Fdead(sim, age, y, fl) = res.FDeadAtAge[age][fl];
          Fret(sim, age, y, fl) = res.FRetainAtAge[age][fl];
        }
      }
    
    } // end sim loop
  } // end stock loop
} 

#endif