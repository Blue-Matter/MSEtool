#ifndef CALC_SPATIAL_DISTRIBUTION_H
#define CALC_SPATIAL_DISTRIBUTION_H

#include <Rcpp.h>
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

/**
 * Calculate spatial effort distribution across areas
 *
 * Computes fleet- and simulation-specific spatial effort shares
 * based on exploitable biomass, saturation, and targeting behaviour.
 *
 * @param y             Time-step index
 * @param NumStockList  List of nStock 4D arrays (sim, age, year, area)
 * @param WeightFleetList List of nStock 4D arrays (sim, age, year, fleet)
 * @param SelList       List of nStock 5D arrays (sim, age, year, fleet, area)
 * @param RetList       List of nStock 5D arrays (sim, age, year, fleet, area)
 * @param q             Catchability array (sim, stock, year, fleet)
 * @param Closure       Area open (1) / closed (0) array (sim, stock, year, fleet, area)
 * @param Targeting     Fleet targeting exponent (sim, year, fleet)
 * @param Effort        Total effort (sim, year, fleet)
 * @param RelSize       Relative area size (sim, area)
 * @param nSim          Number of simulations
 * @param nStock        Number of stocks
 * @param nFleet        Number of fleets
 * @param nArea         Number of spatial areas
 *
 * @return Array3D      Spatial effort distribution (sim, fleet, area)
 */
inline Array3D CalcSpatialDistribution(
  const int y,                              // time-step index
  const  Rcpp::List& NumStockList,           // Number-at-age list nStock of array: sim, age, year, area
  const  Rcpp::List& WeightFleetList,        // Number-at-age list nStock of array: sim, age, year, area
  const  Rcpp::List& SelList,                // Selectivity-at-age list nStock of array: sim, age, year, fleet, area
  const  Rcpp::List& RetList,                // Retention-at-age list nStock of array: sim, age, year, fleet, area
  const  Array4D& q,                          // Catchability array: sim, stock, year, fleet
  const  Array5D& Closure,                    // Area closed (0) or open (1) array: sim, stock, year, fleet, area
  const  Array3D& Targeting,                   // Spatial Targeting: sim, year, fleet
  const  Array3D& Effort,                     // Total Effort: sim, year, fleet
  const  Array2D& RelSize,                    // Relative Area Size; sim, area
  const int nSim,
  const int nStock,
  const int nFleet,
  const int nArea)    {               
  
  
  const std::array<int,3> dim = {nSim, nFleet, nArea};
  
  if (nArea == 1) {
    Array3D EffortDist(dim, 1.0);
    return EffortDist;
  }
  
  // Util array: sim, fleet, area 
  // Stored because we may want to keep this object later
  Array3D Util(dim, 0.0);
  
  // Loop over stocks
  for (int st = 0; st < nStock; ++st) { 
    ConstArrayView4D Num = view_StockList4D(NumStockList, st);
    ConstArrayView4D Wgt = view_StockList4D(WeightFleetList, st);
    ConstArrayView5D Sel = view_StockList5D(SelList, st);
    ConstArrayView5D Ret = view_StockList5D(RetList, st);
    Array3D B_hat(dim, 0.0);

    // Exploitable biomass per unit effort
    for (int sim = 0; sim < nSim; ++sim) {
      for (int fl = 0; fl < nFleet; ++fl) {
        for (int ar = 0; ar < nArea; ++ar) {
          if (Closure(sim, st, y, fl, ar) <= 0.0) continue;
          
          double B_sfr = 0.0;
          for (int age = 0; age < Num.dim[1]; ++age) {
            B_sfr +=
              Num(sim, age, y, ar) *
              Wgt(sim, age, y, fl) *
              Sel(sim, age, y, fl, ar) *
              Ret(sim, age, y, fl, ar);
          }
          B_hat(sim, fl, ar) = q(sim, st, y, fl) * B_sfr;
        }
      }
    } 
 
    // Within-season saturation 
    for (int sim = 0; sim < nSim; ++sim) {
      for (int fl = 0; fl < nFleet; ++fl) {
        const double phi = q(sim, st, y, fl) * Effort(sim, y, fl);
         
        // Median B_ref across areas
        std::vector<double> Bvec(nArea);
        for (int ar = 0; ar < nArea; ++ar)
          Bvec[ar] = B_hat(sim, fl, ar);
         
        std::nth_element(
          Bvec.begin(),
          Bvec.begin() + nArea / 2,
          Bvec.end()
        ); 
        const double Bref = Bvec[nArea / 2];
         
        if (Bref <= 0.0) continue;
         
        // Apply stock-specific utility
        for (int ar = 0; ar < nArea; ++ar) {
          const double B = B_hat(sim, fl, ar);
          const double A = RelSize(sim, ar);
           
          if (A > 0.0 && B > 0.0) {
            const double Gamma = B / (A * Bref);
            Util(sim, fl, ar) += B / (1.0 + phi * Gamma);
          }
        }
      }
    }
  } // end stock loop
  
  // Normalize utility across areas 
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total += Util(sim, fl, ar);
       
      if (total > 0.0) {
        for (int ar = 0; ar < nArea; ++ar)
          Util(sim, fl, ar) /= total;
      } 
    }
  }
  
  // Calculate Effort Distribution
  Array3D EffortDist(dim, 0.0);
  
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      const double theta = Targeting(sim,y,fl);
      if (theta <= 0.0) 
        continue;
      double total = 0.0;
      for (int ar = 0; ar < nArea; ++ar)
        total += std::pow(Util(sim, fl, ar),theta);
      
      if (total > 0.0) {
        for (int ar = 0; ar < nArea; ++ar)
          EffortDist(sim, fl, ar) = std::pow(Util(sim, fl, ar), theta) / total;
      } 
    }
  }
 
  return EffortDist;
}

#endif // CALC_SPATIAL_DISTRIBUTION_H