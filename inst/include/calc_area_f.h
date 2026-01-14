#ifndef CALC_AREA_F_H
#define CALC_AREA_F_H

#include <Rcpp.h>
#include <array>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"


inline void CalcArea_F(
    const int y,                              // time-step index
    Rcpp::List& FDeadAreaList,                // FDead-at-age list nStock of array: sim, age, year, fleet, area
    Rcpp::List& FRetainAreaList,              // FDead-at-age list nStock of array: sim, age, year, fleet, area
    
    const Array3D& Dist,                       // Prob Spatial Effort: sim, fleet, area
    const Array4D& q,                         // Catchability array: sim, stock, year, fleet
    const Array3D& Effort,                    // Total Effort: sim, year, fleet
    const Array2D& RelSize,                   // Relative Area Size; sim, area
    
    const  Rcpp::List& SelList,               // Selectivity-at-age list nStock of array: sim, age, year, fleet, area
    const  Rcpp::List& RetList,               // Retention-at-age list nStock of array: sim, age, year, fleet, area
    const  Rcpp::List& DiscMList,             // Discard-mortality-at-age list nStock of array: sim, age, year, area
    
    const int nSim,
    const int nStock,
    const int nFleet,
    const int nArea)    {  
  
  // single area  & single fleet
  if (nArea == 1 && nFleet == 1) {
    for (int st = 0; st < nStock; ++st) { 
      Array5D FDeadArea = clone_StockList5D(FDeadAreaList, st);     // sim, age, year, fleet, area
      Array5D FRetainArea = clone_StockList5D(FRetainAreaList, st); // sim, age, year, fleet, area
      ConstArrayView5D Sel = view_StockList5D(SelList, st);         // sim, age, year, fleet, area
      ConstArrayView5D Ret = view_StockList5D(RetList, st);         // sim, age, year, fleet, area
      ConstArrayView5D DiscM = view_StockList5D(DiscMList, st);     // sim, age, year, fleet, area
      
      const int nAge = FDeadArea.dim[1];
      
      for (int sim = 0; sim < nSim; ++sim) {
        const double effort = Effort(sim, y, 0);
        const double q_eff  = q(sim, st, y, 0) * effort;
        for (int age = 0; age < nAge; ++age) {
          const double F_interact = q_eff * Sel(sim, age, y, 0, 0);
          const double F_retain = F_interact * Ret(sim, age, y, 0, 0);
          const double F_discard_dead = (F_interact - F_retain) * DiscM(sim, age, y, 0, 0);
          FDeadArea(sim, age, y, 0, 0) = F_retain + F_discard_dead;
          FRetainArea(sim, age, y, 0, 0) = F_retain;
        }
      }
    }  
    return;
  } 
  
  // single area - multiple fleet
  if (nArea == 1) {
    for (int st = 0; st < nStock; ++st) { 
      Array5D FDeadArea = clone_StockList5D(FDeadAreaList, st);     // sim, age, year, fleet, area
      Array5D FRetainArea = clone_StockList5D(FRetainAreaList, st); // sim, age, year, fleet, area
      ConstArrayView5D Sel = view_StockList5D(SelList, st);         // sim, age, year, fleet, area
      ConstArrayView5D Ret = view_StockList5D(RetList, st);         // sim, age, year, fleet, area
      ConstArrayView5D DiscM = view_StockList5D(DiscMList, st);     // sim, age, year, fleet, area
      
      const int nAge = FDeadArea.dim[1];
      
      for (int sim = 0; sim < nSim; ++sim) {
        for (int fl = 0; fl < nFleet; ++fl) {
          
          const double effort = Effort(sim, y, fl);
          const double q_eff  = q(sim, st, y, fl) * effort;
          
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * Sel(sim, age, y, fl, 0);
            const double F_retain = F_interact * Ret(sim, age, y, fl, 0);
            const double F_discard_dead = (F_interact - F_retain) * DiscM(sim, age, y, fl, 0);
            FDeadArea(sim, age, y, fl, 0) = F_retain + F_discard_dead;
            FRetainArea(sim, age, y, fl, 0) = F_retain;
          }
        }
      }
    }
    return;
  }
  
  // single fleet, multiple areas 
  if (nFleet == 1) {
    // Compute spatial effort density
    const std::array<int, 2> dim = {nSim, nArea};
    Array2D EffortDensity(dim, 0.0);
    
    for (int sim = 0; sim < nSim; ++sim) {
      const double E = Effort(sim, y, 0);
      for (int ar = 0; ar < nArea; ++ar) {
        const double rs = RelSize(sim, ar);
        EffortDensity(sim, ar) =  (rs > 0.0) ? E * Dist(sim, 0, ar) / rs : 0.0;
      }
    }
    
    for (int st = 0; st < nStock; ++st) {
      Array5D FDeadArea   = clone_StockList5D(FDeadAreaList, st);
      Array5D FRetainArea = clone_StockList5D(FRetainAreaList, st);
      ConstArrayView5D Sel   = view_StockList5D(SelList, st);
      ConstArrayView5D Ret   = view_StockList5D(RetList, st);
      ConstArrayView5D DiscM = view_StockList5D(DiscMList, st);
    
      const int nAge = FDeadArea.dim[1];
      
      for (int sim = 0; sim < nSim; ++sim) {
        const double q_fl = q(sim, st, y, 0);
        
        for (int ar = 0; ar < nArea; ++ar) {
          const double q_eff = q_fl * EffortDensity(sim, ar);
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * Sel(sim, age, y, 0, ar);
            
            const double F_retain = F_interact * Ret(sim, age, y, 0, ar);
            const double F_discard_dead = (F_interact - F_retain) * DiscM(sim, age, y, 0, ar);
            
            FDeadArea(sim, age, y, 0, ar) = F_retain + F_discard_dead;
            FRetainArea(sim, age, y, 0, ar) = F_retain;
          }
        }
      }
    }
    return;
  } 
  

  // multiple areas, multiple fleets 
  
  // Compute spatial effort density
  const std::array<int, 3> dim = {nSim, nFleet, nArea};
  Array3D EffortDensity(dim, 0.0);
  
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      const double E = Effort(sim, y, fl);
      for (int ar = 0; ar < nArea; ++ar) {
        const double rs = RelSize(sim, ar);
        EffortDensity(sim, fl, ar) =  (rs > 0.0) ? E * Dist(sim, fl, ar) / rs : 0.0;
      }
    }
  }
  
  // Loop over stocks 
  for (int st = 0; st < nStock; ++st) { 
    Array5D FDeadArea = clone_StockList5D(FDeadAreaList, st);     // sim, age, year, fleet, area
    Array5D FRetainArea = clone_StockList5D(FRetainAreaList, st); // sim, age, year, fleet, area
    ConstArrayView5D Sel = view_StockList5D(SelList, st);         // sim, age, year, fleet, area
    ConstArrayView5D Ret = view_StockList5D(RetList, st);         // sim, age, year, fleet, area
    ConstArrayView5D DiscM = view_StockList5D(DiscMList, st);     // sim, age, year, fleet, area
    
    const int nAge = FDeadArea.dim[1];
    
    for (int sim = 0; sim < nSim; ++sim) {
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_fl = q(sim, st, y, fl);
        for (int ar = 0; ar < nArea; ++ar) {
          const double q_eff = q_fl * EffortDensity(sim, fl, ar);
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * Sel(sim, age, y, fl, ar);
            const double F_retain = F_interact * Ret(sim, age, y, fl, ar);
            const double F_discard_dead = (F_interact - F_retain) * DiscM(sim, age, y, fl, ar);
            
            FDeadArea(sim, age, y, fl, ar) = F_retain + F_discard_dead;
            FRetainArea(sim, age, y, fl, ar) = F_retain;
          }
        }
      }
    }
  }
}

#endif // CALC_AREA_F_H