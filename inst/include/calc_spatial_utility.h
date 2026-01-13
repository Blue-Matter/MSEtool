#ifndef MSETOOL_CALC_SPATIAL_UTILITY_H
#define MSETOOL_CALC_SPATIAL_UTILITY_H

#include <Rcpp.h>
#include <vector>

#include "array_types.h"   
#include "array_views.h"   

// ---------------------------------------------------------
// Calculates spatial utility
//
//
// AB_stocks: available biomass by sim × stock × fleet × area
//
// stock_wt:
//   - empty  → equal weighting across stocks
//   - length nStock → weighted aggregation
//
// Returns:
//   EffortFrac: sim × fleet × area
// ---------------------------------------------------------

inline Array3D
CalcSpatialUtility(const Array4D& AB_stocks,             // sim, stock, fleet, area       
                   const std::vector<double>& stock_wt) {   // length nStock (or empty)
  
  // Dimensions
  const int nSim   = AB_stocks.dim[0];
  const int nStock = AB_stocks.dim[1];
  const int nFleet = AB_stocks.dim[2];
  const int nArea  = AB_stocks.dim[3];
  
  if (!stock_wt.empty() && static_cast<int>(stock_wt.size()) != nStock) {
    Rcpp::stop("CalcFleetSpatialEffort: stock_wt length must equal nStock");
  } 
  
  // Output: Spatial utility: sim × fleet × area
  std::array<int, 3> dim = {nSim, nFleet, nArea};
  Array3D Util(dim);
  
  // Aggregate available biomass over stocks
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar) {
        double bio = 0.0;
        for (int st = 0; st < nStock; ++st) {
          const double w = stock_wt.empty() ? 1.0 : stock_wt[st];
          bio += w * AB_stocks(sim, st, fl, ar);
        }
        Util(sim, fl, ar) = bio;
        total += bio;
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

#endif // MSETOOL_CALC_SPATIAL_UTILITY_H
