#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

Array3D CalcSpatialUtility(
  int y,                              // time-step index
  Rcpp::List& NumStockList,           // Number-at-age list nStock of array: sim, age, year, area
  Rcpp::List& WeightFleetList,        // Number-at-age list nStock of array: sim, age, year, area
  Rcpp::List& SelList,                // Selectivity-at-age list nStock of array: sim, age, year, area
  Rcpp::List& RetList,                // Retention-at-age list nStock of array: sim, age, year, area
  Rcpp::List& CatchabilityList,       // Catchability list nStock of array: sim, year, fleet
  Rcpp::List& ClosureList,            // Closure list nStock of array: sim, year, fleet, area (0 or 1)
  Rcpp::List& EffortList,
  Array3D HabitatCapacity, 
  const int nSim,
  const int nStock,
  const int nFleet,
  const int nArea)    {               
  
  // Ouput array: Util: sim, fleet, area 
  std::array<int,3> dim = {nSim, nFleet, nArea};
  Array3D Util(dim, 0.0);
  
  
  // Loop over stocks
  for (int st = 0; st < nStock; ++st) { 
    
    // Stock-specific arrays 
    Array4D Num = clone_StockList4D(NumStockList, st);
    Array4D Wgt = clone_StockList4D(WeightFleetList, st);
    Array5D Sel = clone_StockList5D(SelList, st);
    Array5D Ret = clone_StockList5D(RetList, st);
    Array4D Closure = clone_StockList4D(ClosureList, st);
    Array3D q   = clone_StockList3D(CatchabilityList, st);
  
    // Time-step slices
    Array3D Num_y = slice_year(Num, y);   // sim, age, area
    Array3D Wgt_y = slice_year(Wgt, y);   // sim, age, fleet
    Array4D Sel_y = slice_year(Sel, y);   // sim, age, fleet, area
    Array4D Ret_y = slice_year(Ret, y);   // sim, age, fleet, area
    Array3D Clo_y = slice_year(Closure, y);   // sim, fleet, area
    Array2D q_y   = slice_year(q, y);     // sim, fleet
    
    // Utility contribution
    for (int sim = 0; sim < nSim; ++sim) {
      for (int fl = 0; fl < nFleet; ++fl) {
        for (int ar = 0; ar < nArea; ++ar) {
          
          // catch-rate (available vuln biomass x q)
          double ab = 0.0;
           
          for (int age = 0; age < Num_y.dim[1]; ++age) {
            ab +=
              Num_y(sim, age, ar) *
              Wgt_y(sim, age, fl) *
              Sel_y(sim, age, fl, ar) *
              Ret_y(sim, age, fl, ar) * 
              Clo_y(sim, age, fl, ar);
          }
           
          ab *= q_y(sim, fl);
           
          const double cap = HabitatCapacity(sim, st, ar);
          if (cap <= 0.0 || ab <= 0.0) continue;
           
          // Habitat-scaled, depletion-adjusted utility
          const double B = ab / cap;
          const double U = B / (1.0 + alpha * B);
           
          Util(sim, fl, ar) += U;
        }
      }
    }
  } // end stock loop
  
  // Normalize over areas (per sim × fleet)
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar) {
        total += Util(sim, fl, ar);
      }
      if (total > 0.0) {
        for (int ar = 0; ar < nArea; ++ar) {
          Util(sim, fl, ar) /= total;
        }
      }
    } 
  }

  return Util;
}