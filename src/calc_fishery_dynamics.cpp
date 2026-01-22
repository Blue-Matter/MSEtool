#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "hist_view.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"
#include "calc_spawn_production.h"
#include "calc_recruitment.h"
#include "calc_number_next.h"
#include "calc_biomass.h"
#include "calc_catch.h"
#include "calc_overall_f.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              Rcpp::NumericVector Years, // Years to loop over
                              Rcpp::NumericVector AllYears,
                              int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int DoCalcCatch=1,       // calculate catch?
                              const int DoCalcaggF=1         // calculate overall F?   
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
                            nSim,
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
               nSim,
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
                        nSim,
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
    // Calculate Recruitment & Distribute over areas
    // src: inst/include/calc_recruitment.h
    // ---------------------------------------------------------
    
    CalcRecruitment(y,
                    nSim,
                    hv.Number,
                    hv.SProduction,
                    hv.SRR_Pars,
                    hv.SRR_Model,
                    hv.RecLag,
                    hv.RecDevs,
                    hv.SP0,
                    hv.R0,
                    hv.RecDist,
                    nStock,
                    nArea);
    
    // ---------------------------------------------------------
    // Calculate Number at beginning of next time step
    // src: inst/include/calc_number_next.h
    // ---------------------------------------------------------
    
    CalcNumberNext(y,
                   nSim,
                   hv.Number,
                   hv.FDeadArea,
                   hv.NaturalMortality,
                   hv.Semelparous,
                   hv.PlusGroup,
                   hv.Movement,
                   nStock,
                   nFleet,
                   nArea);
    
    // ---------------------------------------------------------
    // Calculate Biomass (this time step)
    // src: inst/include/calc_biomass.h
    // ---------------------------------------------------------
    
    CalcBiomass(y,
                nSim,
                hv.Biomass,
                hv.Number,
                hv.Weight,
                nStock,
                nArea);
    
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    if (DoCalcCatch) {
      
      // TODO   - calc landings- and discards-at-size
      //        - need to calculate ASK internally
      //        - and first check if sel_len/wght exists
      
      CalcCatch(y,
                nSim,
                hv.LandingsAtAge,
                hv.DiscardsAtAge,
                hv.FDeadArea,
                hv.FRetainArea,
                hv.NaturalMortality,
                hv.Number,
                nStock,
                nFleet,
                nArea);
      
    }
    
    // ---------------------------------------------------------
    // Calculate overall F (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    if (DoCalcaggF) {
      CalcOverallF(y,
                   nSim,
                   hv.FDead,
                   hv.FRetain,
                   hv.FDeadArea,
                   hv.FRetainArea,
                   hv.LandingsAtAge,
                   hv.DiscardsAtAge,
                   hv.SelAge,
                   hv.RetAge,
                   hv.DiscMort,
                   hv.NaturalMortality,
                   hv.Number,
                   hv.WeightFleet,
                   nStock,
                   nFleet,
                   nArea);
    }
    
    
  }
  return(Hist);
}