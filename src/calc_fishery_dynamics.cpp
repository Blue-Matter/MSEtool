#include <Rcpp.h>
#include <cmath>
#include <vector>

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
                              SEXP Years, 
                              SEXP AllYears,
                              std::vector<int> Sims,
                              const int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int DoCalcCatch=1,
                              const int DoCalcSpawnProduction=1,
                              const int DoCalcRecruitment=1,
                              const int DoCalcNumberNext=1,
                              const int DoCalcBiomass=1,
                              const int DoCalcOverallF=1,       
                              const int debug=0,
                              const int clone=1
) {
  
  
  if (debug) 
    Rcpp::Rcout << "Starting CalcFisheryDynamics_\n";
  
  // Checks 
  check_years_argument(Years, "Years");
  NumericVector years(Years);
  
  check_years_argument(AllYears, "AllYears");
  NumericVector all_years(AllYears);
  
  if (nSim < 1) Rcpp::stop("nSim < 1");
  
  if (nStock < 1) Rcpp::stop("nStock < 1");
  
  if (nFleet < 1) Rcpp::stop("nFleet < 1");
  
  if (nArea < 1) Rcpp::stop("nArea < 1");
  
  // zero index Sims
  NormalizeSims(Sims, nSim);
  
  // Clone hist object if required (clone=1, default)
  // clone=0 is faster but mutates HistIn directly - only use when 
  // the caller does not need HistIn preserved after this call
  // Rcpp::S4 Hist = clone ? Rcpp::clone(HistIn) : HistIn;
  
  Rcpp::S4 Hist = clone ? Rcpp::clone(HistIn) : Rcpp::S4(Rf_shallow_duplicate(HistIn));
  
  // Always ensure Effort and Distribution are independent copies,
  if (!clone) {
    SEXP effort_dup = PROTECT(Rf_duplicate(HistIn.slot("Effort")));
    SEXP dist_dup   = PROTECT(Rf_duplicate(HistIn.slot("Distribution")));
    SEXP num_dup    = PROTECT(Rf_duplicate(HistIn.slot("Number")));
    
    Hist.slot("Effort")       = effort_dup;
    Hist.slot("Distribution") = dist_dup;
    Hist.slot("Number")       = num_dup;
    
    UNPROTECT(3);
  }
  
  // create HistView object 
  HistView hv(Hist, nSim, nStock, nFleet, nArea);
  
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(years, all_years); // time-step index
  for (int i : ts_index) {
    if (i < 0 || i >= all_years.size())
      Rcpp::stop("Invalid time index " + std::to_string(i) + "from CalcTSIndex()");
  }
  int nTS = ts_index.size();
  
  if (debug) 
    Rcpp::Rcout << "Starting time step loop \n";

  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
  
    int y = ts_index[ts]; // index for this time step
    
    if (debug) {
      Rcpp::Rcout << "******** Begin Time Step ***************\n";
      Rcpp::Rcout << "ts = " << ts << "\n";
      Rcpp::Rcout << "y = " << y+1 << "\n";
    }
      
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
    
    if (debug) 
      Rcpp::Rcout << "Begin CalcSpatialDistribution \n";
    
    CalcSpatialDistribution(y,
                            Sims,
                            nSim,
                            hv.Distribution,
                            hv.Number,
                            hv.WeightFleetRetained,
                            hv.SelAge,
                            hv.RetAge,
                            hv.q,
                            hv.Closure,
                            hv.Spatial_Targeting,
                            hv.Effort,
                            hv.RelSize,
                            hv.UseDensity,
                            nStock,
                            nFleet,
                            nArea);
    
    if (debug) 
      Rcpp::Rcout << "End CalcSpatialDistribution \n";
    
    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // 
    // Applies maxF constraint within each area 
    // ---------------------------------------------------------
    
    if (debug)
      Rcpp::Rcout << "Begin CalcArea_F \n";

    CalcArea_F(y,
               Sims,
               nSim,
               hv.FInteractArea,
               hv.FDeadArea,
               hv.FRetainArea,
               hv.SelAge,
               hv.RetAge,
               hv.DiscMort,
               hv.Distribution,
               hv.q,
               hv.Effort,
               hv.RelSize,
               hv.StockTargeting,
               hv.StockTargetingFlag,
               hv.maxF,
               nStock,
               nFleet,
               nArea);

    if (debug)
      Rcpp::Rcout << "End CalcArea_F \n";


    
    if (DoCalcSpawnProduction) {
      
      // ---------------------------------------------------------
      // Calculate Global Spawning Biomass and Spawning Production
      // src: inst/include/calc_spawn_production.h
      // ---------------------------------------------------------
      
      if (debug)
        Rcpp::Rcout << "Begin CalcSpawnProduction \n";
      
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
      
      if (debug)
        Rcpp::Rcout << "End CalcSpawnProduction \n";
    }
    
    if (DoCalcRecruitment) {
      
      // ---------------------------------------------------------
      // Calculate Recruitment & Distribute over areas
      // src: inst/include/calc_recruitment.h
      // ---------------------------------------------------------
      
      if (debug)
        Rcpp::Rcout << "Begin CalcRecruitment \n";
      
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
      
      if (debug)
        Rcpp::Rcout << "End CalcRecruitment \n";
    }
    
    if (DoCalcNumberNext) {
      
      // ---------------------------------------------------------
      // Calculate Number at beginning of next time step
      // src: inst/include/calc_number_next.h
      // ---------------------------------------------------------
      
      if (debug)
        Rcpp::Rcout << "Begin CalcNumberNext \n";
      
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
      
      
      if (debug)
        Rcpp::Rcout << "End CalcNumberNext \n";
    }
    

    if (DoCalcBiomass) {
      
      // ---------------------------------------------------------
      // Calculate Biomass (this time step)
      // src: inst/include/calc_biomass.h
      // ---------------------------------------------------------
      
      
      if (debug)
        Rcpp::Rcout << "Begin CalcBiomass \n";
      
      CalcBiomass(y,
                  Sims,
                  nSim,
                  hv.Biomass,
                  hv.Number,
                  hv.Weight,
                  nStock,
                  nArea);
      
      if (debug)
        Rcpp::Rcout << "End CalcBiomass \n";
    }
    

    if (DoCalcCatch) {
      
      // ---------------------------------------------------------
      // Calculate Catch 
      // src: inst/include/calc_catch.h
      // ---------------------------------------------------------
      
      // TODO   - calc landings- and discards-at-size
      //        - need to calculate ASK internally
      //        - and first check if sel_len/wght exists
      
      if (debug)
        Rcpp::Rcout << "Begin CalcCatch \n";

      CalcCatch(y,
                Sims,
                nSim,
                hv.InteractAtAge,
                hv.LandingsAtAge,
                hv.DiscardsAtAge,
                hv.Interactions,
                hv.Landings,
                hv.Discards,
                hv.FInteractArea,
                hv.FDeadArea,
                hv.FRetainArea,
                hv.NaturalMortality,
                hv.Number,
                hv.WeightFleetRetained,
                hv.WeightFleetSelected,
                nStock,
                nFleet,
                nArea);

      if (debug)
        Rcpp::Rcout << "End CalcCatch \n";
      
    }
    
    if (DoCalcOverallF) {
      
      // ---------------------------------------------------------
      // Calculate overall F (if applicable)
      // src: inst/include/cacl_overall_f.h
      // ---------------------------------------------------------
      
      if (debug)
        Rcpp::Rcout << "Begin CalcOverallF \n";
      
      CalcOverallF(y,
                   Sims,
                   nSim,
                   hv.FInteract,
                   hv.FDead,
                   hv.FRetain,
                   hv.FInteractArea,
                   hv.FDeadArea,
                   hv.FRetainArea,
                   hv.Number,
                   nStock,
                   nFleet,
                   nArea
      );
      
      if (debug)
        Rcpp::Rcout << "End CalcOverallF \n";
    }
    
    if (debug) 
      Rcpp::Rcout << "******End Time Step ****\n\n";
  
    
  }
  return(Hist);
}
