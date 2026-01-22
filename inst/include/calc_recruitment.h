#ifndef CALC_RECRUITMENT_H
#define CALC_RECRUITMENT_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"
#include "srr_functions.h"


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
    
    // skip if insufficient room in Number 
    const int rec_y = y + lag; // year index for the recruitment
    if (rec_y >= Num_st.dim[2])
      continue;
    
    for (int sim = 0; sim < nSim; ++sim) {

      const double SP = SProduction(sim, st, y); // spawning production this time step
      
      // Calculate recruitment
      double R = EvalSRR(
        model,
        SP,
        SP0(sim, st, y),
        R0(sim, st, rec_y),
        pars_st,
        sim, 
        y);
    
      R *=  devs(sim, rec_y);
      
      // distribute over areas according to RecDist
      for (int area = 0; area < nArea; ++area) {
        Num_st(sim,0,rec_y,area) = R * RecDist(sim, st, rec_y, area);
      }
      
  
    }
  }
}

#endif
