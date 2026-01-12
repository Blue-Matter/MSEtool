#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "calc_vbiomass.h"
#include "calc_avail_biomass.h"

using namespace Rcpp;

inline Array4D clone_StockList(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = Rcpp::clone(StockList[st]);
  StockList[st] = x;   // optional: reserve slot immediately
  return Array4D(x);
}


// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              Rcpp::NumericVector Years, // Yearsto loop over
                              Rcpp::NumericVector YearsAll, // all Years
                              
                              // Stock 
                              Rcpp::List NumStock,
                              Rcpp::List nAgeList,
                           
                              int nSim,
                              int nStock,
                              int nFleet,
                              int nArea) {

  Rcpp::S4 Hist = clone(HistIn); 
  
  // Examples 
  // // VB: sim × stock × year × fleet × area
  // Array5D VB = Slot2Array5D(Hist, "VBiomass");
  // 
  // // VB_ts: sim × fleet × area
  // Array3D VB_ts = CalcVBiomass_(Num_ts, Sel_ts, Ret_ts, Wgt_ts);
  // 
  // // assign into VB[ , st, yr, , ]
  // assign_3d_into_5d(VB, VB_ts, st, ts);
  
  // Example 
  // // Sel: sim × stock × year × fleet × area
  // Array5D Sel = Slot2Array5D(Hist, "Selectivity");
  // 
  // // Sel_ts: sim × stock × fleet × area
  // Array4D Sel_ts = slice_year(Sel_full, ts);
  // 
  // // assign into all years
  // assign_4d_into_5d(Sel, Sel_ts, ts);
  
 //  antoher example
 // Array3D VB_ts = CalcVBiomass_(Num_ts, Sel_ts, Ret_ts, Wgt_ts);
 // 
 // // sim × fleet × area → sim × stock × year × fleet × area
 // assign_3d_into_5d(VB, VB_ts, st, ts); 
 
 
  // Rcpp::List Misc = Hist.slot("Misc");
  // if (!Misc.containsElementNamed("SelList"))
  //   stop("Hist@Misc$SelList not found");
  // Rcpp::List SelList = Misc["SelList"]; // list nStock of Selectivity arrays
  // Rcpp::List RetList = Misc["RetList"]; // list nStock of Selectivity arrays
  // Rcpp::List WeightFleetList = Misc["WeightFleetList"];  // list nStock of Weight-at-Age arrays
  // 
  // Rcpp::List NumStockList = Hist.slot("Number"); // List of Number-at-Age arrays
  // 
  // 
  // 
  // // Array5D Dist = Slot2Array5D(Hist, "Distribution"); // sim, stock, year, fleet, area
  // Array5D VB = Slot2Array5D(Hist, "VBiomass"); // sim, stock, year, fleet, area
  // 
  // // Create array vectors
  // std::vector<Array4D> Num(nStock);
  // std::vector<Array5D> Sel(nStock);
  // std::vector<Array5D> Ret(nStock);
  // std::vector<Array4D> Wgt(nStock);
  // 
  // // Time Steps 
  // IntegerVector MatchTS = match(Years, YearsAll);
  // int nTS = Years.size();
  // std::vector<int> ts_index(nTS); // loop over Years
  // 
  // for (int ts = 0; ts < nTS; ++ts) {
  //   
  //   // Time Step Logic 
  //   if (MatchTS[ts] == NA_INTEGER)
  //     stop("Year not found in `YearsAll`");
  //   ts_index[ts] = MatchTS[ts] - 1;
  //   int y = ts_index[ts];
  //   
  //   for (int st = 0; st < nStock; ++st) { 
  //     
  //      IMPORTANT TO CREATE FOR ALL OBJECTS 
  //     Array4D Num = clone_StockList(NumStockList, st);
  //     
  //     // modify Num freely
  //     
  //     Hist.slot("Number") = NumStockList;
  //     
  //     
  //     
  //     Num[st] = Array4D(NumStockList[st]);    // sim × age × area
  //     Sel[st] = Array5D(SelList[st]);         // sim × age × fleet × area
  //     Ret[st] = Array5D(RetList[st]);         // sim × age × fleet × area
  //     Wgt[st] = Array4D(WeightFleetList[st]);      // sim × age × fleet
  //     
  //     Array3D Num_ts = Num[st].slice_year(y);       
  //     Array4D Sel_ts = Sel[st].slice_year(y);       
  //     Array4D Ret_ts = Ret.slice_year(y);       
  //     Array3D Wgt_ts = Wgt[st].slice_year(y);       
  //     
  //     // Calculate Vulnerable Biomass
  //     Array3D VB_ts = CalcVBiomass_(Num_ts, Sel_ts, Ret_ts, Wgt_ts);
  //     fill_hist_5d(VB, st, y, VB_ts);
  //     
  //   }
  // } // end loop over Years
  // 
  // 
  // 
  return(Hist);
}

  // Array3D AB3 = CalcAvailBiomass_(Num, Weight, Sel, Ret, q);
  
  
  
  
  // for (int st = 0; st < nStock; ++st) {
  //   
  //   // Calculate Available Biomass
  //   Array4D SpatUtil
  //   
  // }
  
  
  // 
  // 
  // // Spatial Available Biomass (catchability x VB)
  // Rcpp::NumericVector SpatAB(nSim * nStock * nTS * nFleet * nArea); // storage
  // Array5D AB5(SpatAB, nSim, nStock, nTS, nFleet, nArea); // wrap without copy
  // 
  // 
  // 
  // for (int it = 0; it < nTS; ++it) { // loop over time-steps 
  //   int ts = static_cast<int>(TSind[it]); // index for this time-step 
  //   
  //   
  //   Array5D SpatAB()
  // 
  // Array3D AB(nSim, nFleet, nArea, 0.0);
  // 
  // // Calculate Available Biomass by Area 
  // 
  // 
  // for (int s = 0; s < nSim; ++s) {
  //   for (int f = 0; f < nFleet; ++f) {
  //     for (int a = 0; a < nArea; ++a) {
  //       AB5(s, st, ts, f, a) = AB3(s, f, a);
  //     }
  //   }
  // }
  // 
  // 
  // Array3D CalcAvailBiomass_(const Array3D& Num,
  //                           const Array3D& Weight,
  //                           const Array4D& Sel,
  //                           const Array4D& Ret,
  //                           const Array2D& q)
  //   
  //   // Allocate output
  //   Rcpp::NumericVector SpatAB(nSim * nStock * nTS * nFleet * nArea);
  // Array5D AB5(SpatAB, nSim, nStock, nTS, nFleet, nArea);
  // 
  // 
  //     
  //     // Compute (Sim, Fleet, Area)
  //     
  //     
  //     // Insert into 5D array
  //     for (int s = 0; s < nSim; ++s) {
  //       for (int f = 0; f < nFleet; ++f) {
  //         for (int a = 0; a < nArea; ++a) {
  //           AB5(s, st, ts, f, a) = AB3(s, f, a);
  //         }
  //       }
  //     }
  //   }
  // }
  // 
  //   
  // } // end time step loop
  // 
  // 
  // NumericVector SpatUtil(nSim*nFleet*nArea); // 
    
  // Clone to avoid modifying Hist outside function scope
  // Rcpp::S4 HistOut = clone(Hist)
  //   // Calculat
  //   
  //   auto Effort = Slot2Array4D(HistOut, "Effort");
  // auto Dist = Slot2Array5D(HistOut, "Distribution");
  // 
  
//   return(Hist);
// }






