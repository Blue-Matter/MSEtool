#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              Rcpp::NumericVector Years, // Years to loop over
                              
                              int nSim,
                              int nStock,
                              int nFleet,
                              int nArea,
                              int debug=0) {

  Rcpp::S4 Hist = Rcpp::clone(HistIn); 
  Rcpp::S4 OM = Hist.slot("OM");
  Rcpp::NumericVector YearsAll = OM.slot("Years"); // all historical and projection years (time-steps) 
  
  // ---------------------------------------------------------
  // Mutable Hist list
  // ---------------------------------------------------------
  
  // TODO - clone outside the time loop here 
  
  Rcpp::List FDeadAreaList = Hist.slot("FDeadArea"); // List of F-at-Age-Area arrays 
  Rcpp::List FRetainAreaList = Hist.slot("FRetainArea"); // List of F-at-Age-Area arrays
  
  Rcpp::List NumStockList = Hist.slot("Number"); // List of Number-at-Age arrays
  
  Array3D Effort = Slot2Array3D(Hist, "Effort"); // sim, year, fleet
  Array4D Dist = Slot2Array4D(Hist, "Distribution"); // sim, year, fleet, area
  
  // ---------------------------------------------------------
  // Extract non-mutable objects to from Hist@Misc
  // ---------------------------------------------------------
  
  Rcpp::List Misc = Hist.slot("Misc");
  
  // list length nStock, each element an array
  Rcpp::List WeightFleetList = GetMisc_List(Hist, "WeightFleetList");
  Rcpp::List SelList = GetMisc_List(Hist, "SelList"); 
  Rcpp::List RetList = GetMisc_List(Hist, "RetList"); 
  Rcpp::List DiscMList = GetMisc_List(Hist, "DiscMortList"); 
  
  Array4D q = GetMisc_4DArray(Hist, "Catchability"); // sim, stock, year, fleet
  Array5D Closure =  GetMisc_5DArray(Hist, "Closure"); // sim, stock, year, fleet, area
  Array3D Targeting = GetMisc_3DArray(Hist, "Targeting"); // sim, year, fleet
  Array2D RelSize = GetMisc_2DArray(Hist, "RelSize"); // sim, year, fleet
 
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(Years, YearsAll); // time-step index 
  int nTS = ts_index.size();
  
  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
    
    int y = ts_index[ts]; // index for this time step
    
    // ---------------------------------------------------------
    // Calculate Spatial Distribution of Fishing Effort
    // src: inst/include/calc_spatial_effort_dist.h
    // ---------------------------------------------------------
    
    // TODO - drop nSim and calculate internally
    //      - drop other int arguments
    //      - clone arrays outside of time loop
    
    // sim, fleet, area
    Array3D EffortDist = CalcSpatialDistribution(y,               // year index
                                                 NumStockList,    // Number-at-age list nStock of array: sim, age, year, area
                                                 WeightFleetList, // Weight-at-age list nStock of array: sim, age, year, fleet 
                                                 SelList,         // Selectivity-at-age list nStock of array: sim, age, year, area
                                                 RetList,         // Retention-at-age list nStock of array: sim, age, year, area
                                                 q,               // Catchability array: sim, stock, year, fleet
                                                 Closure,         // Area closed (0) or open (1) array: sim, stock, year, fleet, area
                                                 Targeting,       // Spatial Targeting: sim, year, fleet
                                                 Effort,          // Total Effort: sim, year, fleet
                                                 RelSize);         // Relative Area Size: sim, area

    
    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // ---------------------------------------------------------
    
    // CalcArea_F(y,
    //            FDeadAreaList,
    //            FRetainAreaList,
    //            EffortDist,
    //            q,
    //            Effort,
    //            RelSize,
    //            SelList,
    //            RetList,
    //            DiscMList);
    
    // Update Hist Arrays for this time step
    sFA_into_sYFA(Dist, EffortDist, y); // sim, fleet, area into sim, year, fleet, area
    
    
    
  }
  
  // Update Hist Slots 
  // Hist.slot("Number") = NumStockList;
  Hist.slot("FDeadArea") = FDeadAreaList;
  Hist.slot("FRetainArea") = FRetainAreaList;

  return(Hist);
}


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
 
 

  // // Array5D Dist = Slot2Array5D(Hist, "Distribution"); // sim, stock, year, fleet, area
  // Array5D VB = Slot2Array5D(Hist, "VBiomass"); // sim, stock, year, fleet, area
  // 
  // // Create array vectors
  // std::vector<Array4D> Num(nStock);
  // std::vector<Array5D> Sel(nStock);
  // std::vector<Array5D> Ret(nStock);
  // std::vector<Array4D> Wgt(nStock);
  // 

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






