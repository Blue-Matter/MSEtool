#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// TODO - if 1 Area return 1 

Array3D CalcSpatialDistribution(
  int y,                              // time-step index
  Rcpp::List& NumStockList,           // Number-at-age list nStock of array: sim, age, year, area
  Rcpp::List& WeightFleetList,        // Number-at-age list nStock of array: sim, age, year, area
  Rcpp::List& SelList,                // Selectivity-at-age list nStock of array: sim, age, year, area
  Rcpp::List& RetList,                // Retention-at-age list nStock of array: sim, age, year, area
  Array4D q,                          // Catchability array: sim, stock, year, fleet
  Array5D Closure,                    // Area closed (0) or open (1) array: sim, stock, year, fleet, area
  Array3D Effort,                     // Total Effort: sim, year, fleet
  Array2D RelSize,                    // Relative Area Size; sim, area
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
    
    std::array<int,3> dim = {nSim, nFleet, nArea};
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
  
  // --- Normalize across areas ---
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

  return Util;
}