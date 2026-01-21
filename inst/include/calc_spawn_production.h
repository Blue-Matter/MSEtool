#ifndef CALC_SPAWN_PRODUCTION_H
#define CALC_SPAWN_PRODUCTION_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"


inline void CalcSpawnProduction(
    const int y,
    const int nSim,
    Array3D& SBiomass,
    Array3D& SProduction,
    const std::vector<Array4D>& Number,
    const std::vector<ConstArrayView3D>& Fecundity,
    const std::vector<ConstArrayView3D>& Maturity,
    const std::vector<ConstArrayView3D>& Weight,
    const std::vector<ConstArrayView3D>& NaturalMortality,
    const ConstArrayView2D SpawnTimeFrac,
    const ConstArrayView1D SPFrom,  // invariant across sim
    const std::vector<Array5D>& FDeadArea,
    const int nStock,
    const int nFleet,
    const int nArea) {

  if (y < 0 || y >= SProduction.dim[2])
    Rcpp::stop("y out of bounds in CalcSpawnProduction");
  
  

  const int simSTF = SpawnTimeFrac.dim[0];
  
  for (int st = 0; st < nStock; ++st) {
    const auto& Num_st = Number[st];
    const auto& Fec_st = Fecundity[st];
    const auto& Mat_st = Maturity[st];
    const auto& Wt_st  = Weight[st];
    const auto& M_st   = NaturalMortality[st];
    const auto& FDA_st = FDeadArea[st];
    const int nAge = Num_st.dim[1];
  
    const int simN  = Num_st.dim[0];
    const int simF  = Fec_st.dim[0];
    const int simMa = Mat_st.dim[0];
    const int simW  = Wt_st.dim[0];
    const int simM  = M_st.dim[0];
    const int simFD = FDA_st.dim[0];
    for (int sim = 0; sim < nSim; ++sim) {
      
      const int iN  = sim_i(sim, simN);
      const int iF  = sim_i(sim, simF);
      const int iMa = sim_i(sim, simMa);
      const int iW  = sim_i(sim, simW);
      const int iM  = sim_i(sim, simM);
      const int iFD = sim_i(sim, simFD);
      const int iST = sim_i(sim, simSTF);
      double SB = 0.0;
      double SP = 0.0;
      const double spawnFrac = SpawnTimeFrac(iST, st);
      for (int age = 0; age < nAge; ++age) {
        for (int area = 0; area < nArea; ++area) {
          
          double N = Num_st(iN, age, y, area);
          if (spawnFrac > 0.0) {
            double Z = M_st(iM, age, y);
            for (int fl = 0; fl < nFleet; ++fl) {
              Z += FDA_st(iFD, age, y, fl, area);
            }
            if (Z < 0.0)
              Rcpp::stop("Z is negative in CalcSpawnProduction");
            N *= std::exp(-Z * spawnFrac);
          }
          SP += N * Fec_st(iF, age, y);
          SB += N * Wt_st(iW, age, y) * Mat_st(iMa, age, y);
        }
      }
      SProduction(sim, st, y) = SP;
      SBiomass(sim, st, y)    = SB;
    }
  } 
  
  for (int st = 0; st < nStock; ++st) {
    const int fromSt = SPFrom(st) - 1;
    if (fromSt < 0 || fromSt >= nStock) {
      Rcpp::stop("SPFrom[%d] out of range", st + 1);
    }

    for (int sim = 0; sim < nSim; ++sim) {
      SProduction(sim, st, y) = SProduction(sim, fromSt, y);
    }
  }

}


#endif