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
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int debug=0) {

  Rcpp::S4 Hist = Rcpp::clone(HistIn); 
  Rcpp::S4 OM = Hist.slot("OM");
  // all historical and projection years (time-steps) 
  Rcpp::NumericVector YearsAll = OM.slot("Years"); 
  
  // ---------------------------------------------------------
  // Mutable Hist lists 
  // ---------------------------------------------------------
  
  // List of F-at-Age-Area arrays 
  Rcpp::List FDeadAreaList = Hist.slot("FDeadArea");
  
  // List of F-at-Age-Area arrays
  Rcpp::List FRetainAreaList = Hist.slot("FRetainArea"); 
  
  // List of Number-at-Age arrays
  Rcpp::List NumberList = Hist.slot("Number"); 
  
  
  Array3D Effort = Slot2Array3D(Hist, "Effort"); // sim, year, fleet
  Array4D EffortDist = Slot2Array4D(Hist, "Distribution"); // sim, year, fleet, area
  
  // ---- wrap mutable arrays ----
  std::vector<Array5D> FDeadArea;
  std::vector<Array5D> FRetainArea;
  std::vector<Array4D> NumberArea;
  
  FDeadArea.reserve(nStock);
  FRetainArea.reserve(nStock);
  NumberArea.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    FDeadArea.emplace_back(as_ArrayND<5>(FDeadAreaList[st]));
    FRetainArea.emplace_back(as_ArrayND<5>(FRetainAreaList[st]));
    NumberArea.emplace_back(as_ArrayND<4>(NumberList[st]));
  }
  

  // ---------------------------------------------------------
  // Extract non-mutable objects to from Hist@Misc
  // ---------------------------------------------------------
  
  Rcpp::List Misc = Hist.slot("Misc");
  
  // // list length nStock, each element an array
  const Rcpp::List WeightFleetList = Misc["WeightFleetList"];
  const Rcpp::List SelList         = Misc["SelList"];
  const Rcpp::List RetList         = Misc["RetList"];
  const Rcpp::List DiscMList       = Misc["DiscMortList"];

  std::vector<ConstArrayView5D> Sel;
  std::vector<ConstArrayView5D> Ret;
  std::vector<ConstArrayView5D> DiscM;
  std::vector<ConstArrayView4D> WghtFleet;
  
  Sel.reserve(nStock);
  Ret.reserve(nStock);
  DiscM.reserve(nStock);
  WghtFleet.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Sel.emplace_back(view_StockList5D(SelList, st));
    Ret.emplace_back(view_StockList5D(RetList, st));
    DiscM.emplace_back(view_StockList5D(DiscMList, st));
    WghtFleet.emplace_back(view_StockList4D(WeightFleetList, st));
  }
  

  ConstArrayView4D q = GetMisc_ConstArrayView<4>(Hist, "Catchability");
  ConstArrayView5D Closure = GetMisc_ConstArrayView<5>(Hist, "Closure");
  ConstArrayView3D Targeting = GetMisc_ConstArrayView<3>(Hist, "Targeting");
  ConstArrayView2D RelSize = GetMisc_ConstArrayView<2>(Hist, "RelSize");

  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(Years, YearsAll); // time-step index 
  int nTS = ts_index.size();
  
  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
    
    
    // TODO:  MICE calculations need to done here, but will require 
    //        many of the currently `const` biological arrays 
    //        etc to be modified. Will require downstream revisions
    
    int y = ts_index[ts]; // index for this time step
    
    // ---------------------------------------------------------
    // Calculate Spatial Distribution of Fishing Effort
    // src: inst/include/calc_spatial_effort_dist.h
    // ---------------------------------------------------------
    CalcSpatialDistribution(y,    
                            EffortDist,
                            NumberArea,
                            WghtFleet,
                            Sel,
                            Ret,
                            q,               
                            Closure,         
                            Targeting,       
                            Effort,          
                            RelSize,
                            nStock,
                            nFleet,
                            nArea);         

    
    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // ---------------------------------------------------------
    
    CalcArea_F(y,
               FDeadArea,
               FRetainArea,
               Sel,
               Ret,
               DiscM,
               EffortDist,
               q,
               Effort,
               RelSize,
               nStock,
               nFleet,
               nArea);
    
     
    // ---------------------------------------------------------
    // Calculate Global Spawning Biomass and Spawning Production
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // SPFrom for sharing spawning production across stocks
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Recruitment
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate overall F (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Number at beginning of next time step
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    
    
  }
  return(Hist);
}
