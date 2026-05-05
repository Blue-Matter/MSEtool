#ifndef CALC_RECRUITMENT_H
#define CALC_RECRUITMENT_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"
#include "srr_functions.h"


inline void CalcRecruitment(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array4D>& Number,                                   // [stock] sim, age, year, area                              
    const Array3D& SProduction,                                     // sim, stock, year
    const std::vector<std::vector<ConstArrayView2D>>& SRR_Pars,     // [stock] sim, year
    const std::vector<int>& SRR_Model,                              // stock
    const std::vector<int>& RecLag,                                 // stock
    const std::vector<ConstArrayView2D>& RecDevs,                   // [stock]  sim, year
    const ConstArrayView3D& SP0,                                    // sim, stock, year
    const ConstArrayView3D& R0,                                     // sim, stock, year
    const ConstArrayView4D& RecDist,                                // sim, stock, year, area
    const int nStock,
    const int nArea) {
  
  if ((int)Number.size() < nStock ||
      (int)SRR_Pars.size() < nStock ||
      (int)RecDevs.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  check_dims<3>(SProduction, {nSim, nStock, SProduction.dim[2]}, "SProduction", y, 2);
  check_dims<3>(SP0, {nSim, nStock, SP0.dim[2]}, "SP0", y, 2);
  check_dims<3>(R0, {nSim, nStock, R0.dim[2]}, "R0", y, 2);
  check_dims<4>(RecDist, {nSim, nStock, RecDist.dim[2], nArea}, "RecDist", y, 2);
  
  // Calculate Recruitment and distribute over areas according to movement
  
  for (int st = 0; st < nStock; ++st) {
    
    const int model = SRR_Model[st];   
    const int lag  = RecLag[st];  
    const int rec_y     = y + lag;    // year index where recruits appear
    
    const auto& pars_st = SRR_Pars[st];
    const auto& devs = RecDevs[st];
    Array4D& Num_st = Number[st]; // sim, age, year, area
    
    
    if (pars_st.size() < 1) 
      Rcpp::stop("SRR_Pars[[" + std::to_string(st+1) + " ]] has no parameters");
    
    if (rec_y >= Num_st.dim[2]) continue;
    
    if (rec_y >= R0.dim[2])
      Rcpp::stop("rec_y (" + std::to_string(rec_y) +
        ") exceeds R0 year dimension for stock " + std::to_string(st + 1));
    
    if (rec_y >= devs.dim[1])
      Rcpp::stop("rec_y (" + std::to_string(rec_y) +
        ") exceeds RecDevs year dimension for stock " + std::to_string(st + 1));
    
    if (rec_y >= RecDist.dim[2])
      Rcpp::stop("rec_y (" + std::to_string(rec_y) +
        ") exceeds RecDist year dimension for stock " + std::to_string(st + 1));
    
    for (int sim : Sims) {
      
      const int sim_num   = sim_index<4>(sim, Num_st, "Number");
      
      // Skip if age-0 recruits already populated for rec_y 
      // Any non-zero value in any area at age 0, rec_y means this sim is done.
      bool already_populated = false;
      for (int ar = 0; ar < nArea && !already_populated; ++ar) {
        if (Num_st(sim_num, 0, rec_y, ar) > 0.0)
          already_populated = true;
      }
      if (already_populated) continue;
      
      const int sim_prod  = sim_index<3>(sim, SProduction, "SProduction");
      const int sim_sp0   = sim_index<3>(sim, SP0, "SP0");
      const int sim_r0    = sim_index<3>(sim, R0, "R0");
      const int sim_dev   = sim_index<2>(sim, devs, "RecDevs");
      const int sim_rec   = sim_index<4>(sim, RecDist, "RecDist");
   
      const double SP = SProduction(sim_prod, st, y); // spawning production this time step
      const double sp0 = SP0(sim_sp0, st, y);
      const double r0  = R0(sim_r0,   st, rec_y);
      const double dev = devs(sim_dev, rec_y);
      
      // Calculate recruitment
      const double R = EvalSRR(model, SP, sp0, r0, pars_st, sim, y) * dev;
      
      // distribute over areas according to RecDist
      for (int area = 0; area < nArea; ++area) {
        Num_st(sim_num,0,rec_y,area) = R * RecDist(sim_rec, st, rec_y, area);
      }
      
  
    }
  }
}

#endif
