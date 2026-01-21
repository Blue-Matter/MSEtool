#ifndef CALC_RECRUITMENT_H
#define CALC_RECRUITMENT_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"
#include "srr_functions.h"


// SRR model codes
constexpr int SRR_BEVERTON_HOLT = 0;
constexpr int SRR_RICKER        = 1;
constexpr int SRR_HOCKEY_STICK  = 2;


inline double EvalSRR(
    int model,
    double S,
    double S0,
    double R0,
    const std::vector<ConstArrayView2D>& pars_st,
    int ip,   // sim index for parameters
    int y) {
  
  if (pars_st.empty()) {
    Rcpp::stop("SRR model %d called with zero parameters", model);
  }
  
  switch (model) {
   
  case SRR_BEVERTON_HOLT:
    return BevertonHolt_kernel(
      S,
      S0,
      R0,
      pars_st[0](ip, y) // h
    );
    
  case SRR_RICKER:
    return Ricker_kernel(
      S,
      S0,
      R0,
      pars_st[0](ip, y) // hR
    );
    
  case SRR_HOCKEY_STICK:
    return HockeyStick_kernel(
      S,
      S0,
      R0,
      pars_st[0](ip, y) // Shinge
    );
     
  default:
    Rcpp::stop("Unknown SRR model code");
  }
} 


inline void CalcRecruitment(const int y,
                            const int nSim,
                            std::vector<Array4D>& Number,
                            const Array3D& SProduction,
                            const std::vector<std::vector<ConstArrayView2D>>& SRR_Pars,
                            const std::vector<int>& SRR_Model,
                            const std::vector<int>& RecLag,
                            const std::vector<ConstArrayView2D>& RecDevs,
                            const ConstArrayView3D& SP0,
                            const ConstArrayView3D& R0,
                            const ConstArrayView4D& RecDist,
                            const int nStock,
                            const int nArea) {
  
  if (y < 0 || y >= SProduction.dim[2])
    Rcpp::stop("Year index y out of bounds in CalcRecruitment");
  
  
  const int simSP0  = SP0.dim[0];
  const int simR0   = R0.dim[0];
  const int simRD   = RecDist.dim[0];
  
  // Calculate Recruitment and distribute over areas according to movement
  
  for (int st = 0; st < nStock; ++st) {
    const int model = SRR_Model[st];   // SRR model
    const int lag  = RecLag[st];  
    const auto& pars_st = SRR_Pars[st];
    
    if (pars_st.size() < 1) {
      Rcpp::stop("SRR_Pars[[%d]] has no parameters", st + 1);
    }
    
    const auto& devs = RecDevs[st];
    Array4D& Num_st = Number[st]; // sim, age, year, area
    
    const int simDev = devs.dim[0];
    
    // skip if insufficient room in Number 
    const int rec_y = y + lag; // year index for the recruitment
    if (rec_y >= Num_st.dim[2])
      continue;
    
    const int simPar = pars_st[0].dim[0]; // sim index for parameters 
    
    for (int sim = 0; sim < nSim; ++sim) {
      
      const int iSP0 = sim_i(sim, simSP0);
      const int iR0  = sim_i(sim, simR0);
      const int iRD  = sim_i(sim, simRD);
      const int iDev = sim_i(sim, simDev);
      const int iPar = sim_i(sim, simPar);
      
      const double SP = SProduction(sim, st, y); // spawning production this time step
      
      // Calculate recruitment
      double R = EvalSRR(
        model,
        SP,
        SP0(iSP0, st, y),
        R0(iR0, st, rec_y),
        pars_st,
        iPar, 
        y);
      
      // apply log-normal rec devs
      R *= devs(iDev, y);
      
      // distribute over areas according to RecDist
      for (int area = 0; area < nArea; ++area) {
        Num_st(sim,0,rec_y,area) = R * RecDist(iRD, st, y, area);
      }
      
  
    }
  }
}

#endif
