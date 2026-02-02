#ifndef CALC_SPAWN_PRODUCTION_H
#define CALC_SPAWN_PRODUCTION_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"


inline void CalcSpawnProduction(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    Array3D& SBiomass,                                      // sim, stock, year
    Array3D& SProduction,                                   // sim, stock, year
    const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const std::vector<ConstArrayView3D>& Fecundity,         // [stock] sim, age, year
    const std::vector<ConstArrayView3D>& Maturity,          // [stock] sim, age, year
    const std::vector<ConstArrayView3D>& Weight,            // [stock] sim, age, year
    const std::vector<ConstArrayView3D>& NaturalMortality,  // [stock] sim, age, year
    const ConstArrayView2D SpawnTimeFrac,                   // sim, stock
    const ConstArrayView1D SPFrom,                          // stock
    const std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    const int nStock,
    const int nFleet,
    const int nArea) {

  // Checks
  if ((int)Number.size() < nStock ||
      (int)Fecundity.size() < nStock ||
      (int)Maturity.size() < nStock ||
      (int)Weight.size() < nStock ||
      (int)NaturalMortality.size() < nStock ||
      (int)FDeadArea.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  check_dims<3>(SBiomass, {nSim, nStock, SBiomass.dim[2]}, "SBiomass", y, 2);
  check_dims<3>(SProduction, {nSim, nStock, SProduction.dim[2]}, "SProduction", y, 2);
  check_dims<2>(SpawnTimeFrac, {nSim, nStock}, "SpawnTimeFrac");
  check_dims<1>(SPFrom, {nStock}, "SPFrom");

  for (int st = 0; st < nStock; ++st) {
    const auto& Num_st = Number[st];
    const auto& Fec_st = Fecundity[st];
    const auto& Mat_st = Maturity[st];
    const auto& Wt_st  = Weight[st];
    const auto& M_st   = NaturalMortality[st];
    const auto& FDA_st = FDeadArea[st];
    const int nAge = Num_st.dim[1];
    
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    check_dims<3>(Fec_st, {nSim, nAge, Fec_st.dim[2]}, "Fecundity", y, 2);
    check_dims<3>(Mat_st, {nSim, nAge, Mat_st.dim[2]}, "Maturity", y, 2);
    check_dims<3>(Wt_st, {nSim, nAge, Wt_st.dim[2]}, "Weight", y, 2);
    check_dims<3>(M_st, {nSim, nAge, M_st.dim[2]}, "NaturalMortality", y, 2);
    check_dims<5>(FDA_st, {nSim, nAge, FDA_st.dim[2], nFleet, nArea}, "FDeadArea", y, 2);
    
  
    for (int sim : Sims) {
      
      const int sim_num  = sim_index<4>(sim, Num_st, "Number");
      const int sim_fec  = sim_index<3>(sim, Fec_st, "Fecundity");
      const int sim_mat  = sim_index<3>(sim, Mat_st, "Maturity");
      const int sim_wt   = sim_index<3>(sim, Wt_st, "Weight");
      const int sim_m    = sim_index<3>(sim, M_st, "NaturalMortality");
      const int sim_fda  = sim_index<5>(sim, FDA_st, "FDeadArea");
      const int sim_stf  = sim_index<2>(sim, SpawnTimeFrac, "SpawnTimeFrac");
      
      double SB = 0.0;
      double SP = 0.0;
      
      const double spawnFrac = SpawnTimeFrac(sim_stf, st);
      for (int age = 0; age < nAge; ++age) {
        for (int area = 0; area < nArea; ++area) {
          
          double N = Num_st(sim_num, age, y, area);
          if (spawnFrac > 0.0) {
            double Z = M_st(sim_m, age, y);
            for (int fl = 0; fl < nFleet; ++fl) {
              Z += FDA_st(sim_fda, age, y, fl, area);
            }
            if (Z < 0.0)
              Rcpp::stop("Z is negative in CalcSpawnProduction");
            N *= std::exp(-Z * spawnFrac);
          }
          SP += N * Fec_st(sim_fec, age, y);
          SB += N * Wt_st(sim_wt, age, y) * Mat_st(sim_mat, age, y);
        }
      }
      SProduction(sim, st, y) = SP;
      SBiomass(sim, st, y)    = SB;
    }
  } 
  
  for (int st = 0; st < nStock; ++st) {
    const int fromSt = SPFrom(st) - 1;
    if (fromSt < 0 || fromSt >= nStock) {
      Rcpp::stop("SPFrom" + std::to_string(st+1) + " out of range");
    }

    for (int sim : Sims) {
      SProduction(sim, st, y) = SProduction(sim, fromSt, y);
    }
  }

}


#endif