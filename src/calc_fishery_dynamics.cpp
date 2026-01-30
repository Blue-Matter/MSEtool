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

inline void NormalizeSims(std::vector<int>& Sims, int nSim) {
  for (int& s : Sims) {
    if (s < 1 || s > nSim)
      Rcpp::stop("Sims contains out-of-range index");
    --s;
  }
}

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              SEXP Years, // Years to loop over
                              SEXP AllYears,
                              std::vector<int> Sims,
                              const int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int DoCalcCatch=1,       // calculate catch?
                              const int DoCalcaggF=1         // calculate overall F?   
) {
  
  // Checks 
  check_years_argument(Years, "Years");
  NumericVector years(Years);
  
  check_years_argument(AllYears, "AllYears");
  NumericVector all_years(AllYears);
  
  if (nSim < 1)
    Rcpp::stop("nSim < 1");
  
  if (nStock < 1)
    Rcpp::stop("nStock < 1");
  
  if (nFleet < 1)
    Rcpp::stop("nFleet < 1");
  
  if (nArea < 1)
    Rcpp::stop("nArea < 1");
  
  // zero index Sims
  NormalizeSims(Sims, nSim);
  
  
  // Clone hist object
  Rcpp::S4 Hist = Rcpp::clone(HistIn); 
  
  // create HistView object 
  HistView hv(Hist, nSim, nStock, nFleet, nArea);
  
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(years, all_years); // time-step index
  for (int i : ts_index) {
    if (i < 0 || i >= all_years.size())
      Rcpp::stop("Invalid time index " + std::to_string(i) + "from CalcTSIndex()");
  }
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
                            Sims,
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
               Sims,
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
               hv.maxF,
               nStock,
               nFleet,
               nArea);
    
    // ---------------------------------------------------------
    // Calculate Global Spawning Biomass and Spawning Production
    // src: inst/include/calc_spawn_production.h
    // ---------------------------------------------------------
    
    CalcSpawnProduction(y,
                        Sims,
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
                    Sims,
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
                   Sims,
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
                Sims,
                nSim,
                hv.Biomass,
                hv.Number,
                hv.Weight,
                nStock,
                nArea);
    
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/calc_catch.h
    // ---------------------------------------------------------
    
    
    if (DoCalcCatch) {
      
      // TODO   - calc landings- and discards-at-size
      //        - need to calculate ASK internally
      //        - and first check if sel_len/wght exists
      
      CalcCatch(y,
                Sims,
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
    // src: inst/include/cacl_overall_f.h
    // ---------------------------------------------------------
    
    if (DoCalcaggF) {
      CalcOverallF(y,
                   Sims,
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