#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "hist_view.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"
#include "calc_spawn_production.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              Rcpp::NumericVector Years, // Years to loop over
                              Rcpp::NumericVector AllYears,
                              int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int debug=0,
                              const int CalcCatch=1       // calculate catch and overall F?
) {
  
  Rcpp::S4 Hist = Rcpp::clone(HistIn); 
  HistView hv(Hist, nSim, nStock, nFleet, nArea);
  
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(Years, AllYears); // time-step index 
  int nTS = ts_index.size();
  
  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
    
    int y = ts_index[ts]; // index for this time step
    
    // ---------------------------------------------------------
    // MICE Calculations - TODO 
    // 
    //  Note: currently all biological arrays are non-mutable
    //        and no life-history parameters are passed into the function.
    //        
    //        This will have to be revised once MICE calcs are added
    //
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Spatial Distribution of Fishing Effort
    // src: inst/include/calc_spatial_effort_dist.h
    // ---------------------------------------------------------
    
    CalcSpatialDistribution(y,
                            hv.Distribution,
                            hv.Number,
                            hv.WeightFleet,
                            hv.SelAge,
                            hv.RetAge,
                            hv.q,
                            hv.Closure,
                            hv.Targeting,
                            hv.Effort,
                            hv.RelSize,
                            nStock,
                            nFleet,
                            nArea);
    
    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // ---------------------------------------------------------
    
    CalcArea_F(y,
               hv.FDeadArea,
               hv.FRetainArea,
               hv.SelAge,
               hv.RetAge,
               hv.DiscMort,
               hv.Distribution,
               hv.q,
               hv.Effort,
               hv.RelSize,
               nStock,
               nFleet,
               nArea);
    
    // ---------------------------------------------------------
    // Calculate Global Spawning Biomass and Spawning Production
    // src: inst/include/calc_spawn_production.h
    // ---------------------------------------------------------
    
    CalcSpawnProduction(y,
                        hv.SBiomass,
                        hv.SProduction,
                        hv.Number,
                        hv.Fecundity,
                        hv.Maturity,
                        hv.Weight,
                        hv.NaturalMortality,
                        hv.SpawnTimeFrac,
                        hv.SPFrom,
                        hv.FDeadArea,
                        nStock,
                        nFleet,
                        nArea);
    
    // ---------------------------------------------------------
    // Calculate Recruitment
    // src: inst/include/...
    // ---------------------------------------------------------
    
    // CalcRecruitment(y,
    //                 )
    
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