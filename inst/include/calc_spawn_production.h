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
    const std::vector<int>& SPFromToStock,                  // [pair], 0-indexed
    const std::vector<int>& SPFromFromStock,                // [pair], 0-indexed
    const std::vector<double>& SPFromWeight,                // [pair]
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
  
  
  const int nPair = static_cast<int>(SPFromToStock.size());
  std::vector<double> SP_raw(nStock), SP_new(nStock);

  for (int sim : Sims) {
    for (int st = 0; st < nStock; ++st)
      SP_raw[st] = SProduction(sim, st, y);

    std::fill(SP_new.begin(), SP_new.end(), 0.0);

    for (int p = 0; p < nPair; ++p) {
      const int to   = SPFromToStock[p];
      const int from = SPFromFromStock[p];
      if (to < 0 || to >= nStock || from < 0 || from >= nStock)
        Rcpp::stop("SPFrom pair " + std::to_string(p+1) + " out of range");
      SP_new[to] += SPFromWeight[p] * SP_raw[from];
    }

    for (int st = 0; st < nStock; ++st)
      SProduction(sim, st, y) = SP_new[st];
  }
}

#endif