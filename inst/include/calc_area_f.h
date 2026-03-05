#ifndef CALC_AREA_F_H
#define CALC_AREA_F_H

#include <Rcpp.h>
#include <array>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcArea_F(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array5D>& FInteractArea,              // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
    std::vector<Array5D>& FRetainArea,                // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& SelAge,      // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& RetAge,      // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& DiscMort,    // [stock] sim, age, year, fleet, area
    const Array4D& Distribution,                      // sim, year, fleet, area
    const ConstArrayView4D& q,                        // sim, stock, year, fleet          
    const Array3D& Effort,                            // sim, year, fleet
    const ConstArrayView2D& RelSize,                  // sim, area
    const double maxF,
    const int nStock,
    const int nFleet,
    const int nArea) {


  // Checks
  if ((int)FInteractArea.size() < nStock ||
      (int)FDeadArea.size() < nStock ||
      (int)FRetainArea.size() < nStock ||
      (int)SelAge.size() < nStock ||
      (int)RetAge.size() < nStock ||
      (int)DiscMort.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  check_dims<4>(Distribution, {nSim, Distribution.dim[1], nFleet, nArea}, "Distribution", y, 1);
  check_dims<4>(q, {nSim, nStock, q.dim[2], nFleet}, "q", y, 2);
  check_dims<3>(Effort, {nSim, Effort.dim[1], nFleet}, "Effort", y, 1);
  check_dims<2>(RelSize, {nSim, nArea}, "RelSize");
  

  // Calc F-at-age 
  for (int st = 0; st < nStock; ++st) {
    
    auto& Fi  = FInteractArea[st];    // sim, age, year, fleet, area
    auto& Fd  = FDeadArea[st];        // sim, age, year, fleet, area
    auto& Fr  = FRetainArea[st];      // sim, age, year, fleet, area
    
    const auto& S  = SelAge[st];      // sim, age, year, fleet, area
    const auto& R  = RetAge[st];      // sim, age, year, fleet, area
    const auto& DM = DiscMort[st];    // sim, age, year, fleet, area
    
    const int nAge = Fd.dim[1];
    
    check_dims<5>(S, {nSim, nAge, S.dim[2], nFleet, nArea}, "SelAge", y, 2);
    check_dims<5>(R, {nSim, nAge, R.dim[2], nFleet, nArea}, "RetAge", y, 2);
    check_dims<5>(DM, {nSim, nAge, DM.dim[2], nFleet, nArea}, "DiscMort", y, 2);
    
    for (int sim : Sims) {
      
      const int sim_q   = sim_index<4>(sim, q, "q");
      const int sim_sel = sim_index<5>(sim, S, "S");
      const int sim_ret = sim_index<5>(sim, R, "R");
      const int sim_dm  = sim_index<5>(sim, DM, "DiscMort");
      const int sim_ef  = sim_index<3>(sim, Effort, "Effort");
      const int sim_dist  = sim_index<4>(sim, Distribution, "Distribution");
      const int sim_rs = sim_index<2>(sim, RelSize, "RelSize");
      
      for (int fl = 0; fl < nFleet; ++fl) {

        const double q_fl = q(sim_q, st, y, fl);
        if (q_fl <= 0.0) continue; 
        const double E = Effort(sim_ef, y, fl);
      
        for (int ar = 0; ar < nArea; ++ar) {

          const double rs = RelSize(sim_rs, ar);
          // Effort density
          const double ed = (rs > 0.0) ? E * Distribution(sim_dist, y, fl, ar) / rs : 0.0;
          const double q_eff = q_fl * ed;
          
          if (q_eff <= 0.0) continue;
          
          for (int age = 0; age < nAge; ++age) {
            
            double &Fi_val = Fi(sim, age, y, fl, ar);
            double &Fd_val = Fd(sim, age, y, fl, ar);
            double &Fr_val = Fr(sim, age, y, fl, ar);
            
            const double sel = S(sim_sel, age, y, fl, ar);
            const double ret = R(sim_ret, age, y, fl, ar);
            const double dm  = DM(sim_dm, age, y, fl, ar);
          
            const double F_interact = std::min(q_eff * sel, maxF);
            const double F_retain = F_interact * ret;
            const double F_disc   = (F_interact - F_retain) * dm;
            
            Fi_val = F_interact;
            Fd_val = F_retain + F_disc;
            Fr_val = F_retain;
            
            
          } // end age
        } // end area
      } // end fleet
  
    } // end sim
  } // end stock 

}


#endif // CALC_AREA_F_H