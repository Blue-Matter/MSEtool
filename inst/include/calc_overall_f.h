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
    std::vector<Array4D>& FRetain,                          // sim, age, year, fleet
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& FRetainArea,                // [stock] sim, age, year, fleet, area  
    const std::vector<Array5D>& LandingsAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<Array5D>& DiscardsAtAge,              // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& SelAge,            // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& RetAge,            // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& DiscMort,          // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& FleetWeight,           // [stock] sim, age, year, fleet
    const int nStock,
    const int nFleet,
    const int nArea
) {
  
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];                  // sim, age, year, area
    const auto& Fdead_area = FDeadArea[st];           // sim, age, year, fleet, area
    const auto& Fret_area = FRetainArea[st];          // sim, age, year, fleet, area
    const auto& LAA_st = LandingsAtAge[st];           // sim, age, year, fleet, area
    const auto& DAA_st = DiscardsAtAge[st];           // sim, age, year, fleet, area
    const auto& M_st = NaturalMortality[st];          // sim, age, year
    const auto& FWght_st = FleetWeight[st];           // sim, age, fleet, year 
    const auto& S  = SelAge[st];                      // sim, age, year, fleet, area
    const auto& R  = RetAge[st];                      // sim, age, year, fleet, area
    const auto& DM = DiscMort[st];                    // sim, age, year, fleet, area
    
    auto& Fdead = FDead[st];                          // sim, age, year, fleet
    auto& Fret = FRetain[st];                         // sim, age, year, fleet
    
    const int nAge = Num_st.dim[1];
    
    check_dims<4>(Fdead, {nSim, nAge, Fdead.dim[2], nFleet}, "FDead", y, 2);
    check_dims<4>(Fret, {nSim, nAge, Fret.dim[2], nFleet}, "FRetain", y, 2);
    check_dims<5>(Fdead_area, {nSim, nAge, Fdead_area.dim[2], nFleet, nArea}, "FDeadArea", y, 2);
    check_dims<5>(Fret_area, {nSim, nAge, Fret_area.dim[2], nFleet, nArea}, "FRetainArea", y, 2);
    check_dims<5>(LAA_st, {nSim, nAge, LAA_st.dim[2], nFleet, nArea}, "LandingsAtAge", y, 2);
    check_dims<5>(DAA_st, {nSim, nAge, DAA_st.dim[2], nFleet, nArea}, "DiscardsAtAge", y, 2);
    check_dims<5>(S, {nSim, nAge, S.dim[2], nFleet, nArea}, "SelAge", y, 2);
    check_dims<5>(R, {nSim, nAge, R.dim[2], nFleet, nArea}, "RetAge", y, 2);
    check_dims<5>(DM, {nSim, nAge, DM.dim[2], nFleet, nArea}, "DiscMort", y, 2);
    check_dims<3>(M_st, {nSim, nAge, M_st.dim[2]}, "NaturalMortality", y, 2);
    check_dims<4>(FWght_st, {nSim, nAge, FWght_st.dim[2], nFleet}, "FleetWeight", y, 2);
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    
    for (int sim : Sims) {
      
      
      
      const int sim_num     = sim_index<4>(sim, Num_st, "Number");
      const int sim_fd_area = sim_index<5>(sim, Fdead_area, "FDeadArea");
      const int sim_fr_area = sim_index<5>(sim, Fret_area, "FRetainArea");
      const int sim_LAA     = sim_index<5>(sim, LAA_st, "LandingsAtAge");
      const int sim_DAA     = sim_index<5>(sim, DAA_st, "DiscardsAtAge");
      const int sim_S       = sim_index<5>(sim, S, "SelAge");
      const int sim_R       = sim_index<5>(sim, R, "RetAge");
      const int sim_DM      = sim_index<5>(sim, DM, "DiscMort");
      const int sim_M       = sim_index<3>(sim, M_st, "NaturalMortality");
      const int sim_FWght   = sim_index<4>(sim, FWght_st, "FleetWeight");
      const int sim_Fdead   = sim_index<4>(sim, Fdead, "FDead");
      const int sim_Fret    = sim_index<4>(sim, Fret, "FRetain");
      
      // single-area 
      if (nArea == 1) {
        for (int fl = 0; fl < nFleet; ++fl) {
          for (int age = 0; age < nAge; ++age) {
            Fdead(sim_Fdead, age, y, fl) = Fdead_area(sim_fd_area, age, y, fl, 0);
            Fret(sim_Fret, age, y, fl)   = Fret_area(sim_fr_area, age, y, fl, 0);
          }
        } 
        continue;
      } 
      
      // multiple areas -> aggregate and solve for overall F
      std::vector<double> Nage_total(nAge, 0.0);
      std::vector<double> Removals_total(nFleet, 0.0);
      
      // Sum numbers across areas
      for (int age = 0; age < nAge; ++age) {
        Nage_total[age] = 0.0;
        for (int area = 0; area < nArea; ++area) {
          Nage_total[age] += Num_st(sim_num, age, y, area);
        }
      }
      
      // FleetWeight and NaturalMortality vectors
      std::vector<double> FWght_sim(nAge * nFleet);
      std::vector<double> M_sim(nAge);
      for (int age = 0; age < nAge; ++age) {
        M_sim[age] = M_st(sim_M, age, y);
        for (int fl = 0; fl < nFleet; ++fl)
          FWght_sim[age * nFleet + fl] = FWght_st(sim_FWght, age, y, fl);
      }
      
      // total removals
      for (int fl = 0; fl < nFleet; ++fl) {
        double totalRem = 0.0;
        for (int age = 0; age < nAge; ++age) {
          double weight = FWght_sim[age * nFleet + fl];
          double numRem = 0.0;
          for (int area = 0; area < nArea; ++area)
            numRem += LAA_st(sim_LAA, age, y, fl, area) + DAA_st(sim_DAA, age, y, fl, area);
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
            double Fda = Fdead_area(sim_fd_area, age, y, fl, area);
            selSum  += Fda * S(sim_S, age, y, fl, area);
            retSum  += Fda * R(sim_R, age, y, fl, area);
            discSum += Fda * DM(sim_DM, age, y, fl, area);
            FdeadSum += Fda;
          }
          if (FdeadSum > 0.0) {
            SelMat[age][fl]  = selSum / FdeadSum;
            RetMat[age][fl]  = retSum / FdeadSum;
            DiscMat[age][fl] = discSum / FdeadSum;
          }
        }
      }
      
      SolveFResult res = SolveForF(Nage_total,
                                   Removals_total,
                                   SelMat,
                                   RetMat,
                                   DiscMat,
                                   FWght_sim,
                                   M_sim);
      
      for (int age = 0; age < nAge; ++age)
        for (int fl = 0; fl < nFleet; ++fl) {
          Fdead(sim_Fdead, age, y, fl) = res.FDeadAtAge[age][fl];
          Fret(sim_Fret, age, y, fl)   = res.FRetainAtAge[age][fl];
        }
        
    } // end sim loop
  } // end stock loop
}   

#endif