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

  // survival buffer: survival(age, area) = exp(-Z * spawnFrac)
  std::vector<double> surv_buf;
  
  // loop over stocks
  for (int st = 0; st < nStock; ++st) {
    
    const auto& Num_st = Number[st];
    const auto& Fec_st = Fecundity[st];
    const auto& Mat_st = Maturity[st];
    const auto& Wt_st  = Weight[st];
    const auto& M_st   = NaturalMortality[st];
    const auto& FDA_st = FDeadArea[st];
    
    const int nAge = Num_st.dim[1];
    
    surv_buf.resize(nAge * nArea);
    
    // loop over sims
    for (int sim : Sims) {
      
      SProduction(sim, st, y) = 0.0;
      SBiomass(sim, st, y)    = 0.0;
      
      const int sim_num  = sim_index<4>(sim, Num_st);
      const int sim_fec  = sim_index<3>(sim, Fec_st);
      const int sim_mat  = sim_index<3>(sim, Mat_st);
      const int sim_wt   = sim_index<3>(sim, Wt_st);
      const int sim_m    = sim_index<3>(sim, M_st);
      const int sim_fda  = sim_index<5>(sim, FDA_st);
      const int sim_stf  = sim_index<2>(sim, SpawnTimeFrac);
      
      const double spawnFrac = SpawnTimeFrac(sim_stf, st);
      const bool   doSurv    = spawnFrac > 0.0;

      // compute survival 
      if (doSurv) {
        for (int age = 0; age < nAge; ++age) {
          const double M = M_st(sim_m, age, y); 
          for (int ar = 0; ar < nArea; ++ar) {
            double F_sum = 0.0;
            for (int fl = 0; fl < nFleet; ++fl)
              F_sum += FDA_st(sim_fda, age, y, fl, ar);
            const double Z = M + F_sum;
            surv_buf[age * nArea + ar] = std::exp(-Z * spawnFrac);
          }
        }
      }
      
      double SB = 0.0;
      double SP = 0.0;
      
      if (doSurv) {
        for (int age = 0; age < nAge; ++age) {
          const double fec    = Fec_st(sim_fec, age, y);
          const double wt_mat = Wt_st(sim_wt, age, y) * Mat_st(sim_mat, age, y);
           
          for (int ar = 0; ar < nArea; ++ar) {
            const double N = Num_st(sim_num, age, y, ar) * surv_buf[age * nArea + ar];
            SP += N * fec;
            SB += N * wt_mat;
          }
        } 
      } else {
        for (int age = 0; age < nAge; ++age) {
          const double fec    = Fec_st(sim_fec, age, y);
          const double wt_mat = Wt_st(sim_wt, age, y) * Mat_st(sim_mat, age, y);
          
          for (int ar = 0; ar < nArea; ++ar) {
            const double N = Num_st(sim_num, age, y, ar);
            SP += N * fec;
            SB += N * wt_mat;
          }
          
        }
      } 

      SProduction(sim, st, y) = SP;
      SBiomass(sim, st, y)    = SB;
    } // end sim loop
  } // end stock loop 
  
  
  for (int st = 0; st < nStock; ++st) {
    const int fromSt = static_cast<int>(SPFrom(st)) - 1;  // 1-indexed in R
    
    if (fromSt < 0 || fromSt >= nStock) {
      Rcpp::stop("SPFrom" + std::to_string(st+1) + " out of range");
    }
    

    if (fromSt == st) continue;
    
    for (int sim : Sims) {
      SProduction(sim, st, y) = SProduction(sim, fromSt, y);
    }
  }
}

#endif