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
#include "back_calculate_effort.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              SEXP Years, // Years to loop over
                              SEXP AllYears,
                              std::vector<int> Sims,
                              const int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int DoCalcCatch=1,        // calculate catch?
                              const int debug=0
) {
  
  
  if (debug) 
    Rcpp::Rcout << "Starting CalcFisheryDynamics_\n";
  
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
  
  // Rcpp::S4 Hist(HistIn.get__());
  // Hist.slot("OM") =  Rcpp::clone(as<S4>(HistIn.slot("OM")));
  // Hist.slot("Unfished") =   Rcpp::clone(as<S4>(HistIn.slot("Unfished")));
  // Hist.slot("Misc") =  Rcpp::clone(as<List>(HistIn.slot("Misc")));
  // Hist.slot("Number") =  Rcpp::clone(as<List>(HistIn.slot("Number")));
  // Hist.slot("Biomass") = Rcpp::clone(as<NumericVector>(HistIn.slot("Biomass")));
  // Hist.slot("SBiomass") = Rcpp::clone(as<NumericVector>(HistIn.slot("SBiomass")));
  // Hist.slot("SProduction") = Rcpp::clone(as<NumericVector>(HistIn.slot("SProduction")));
  // Hist.slot("Interactions") = Rcpp::clone(as<NumericVector>(HistIn.slot("Interactions")));
  // Hist.slot("Landings") = Rcpp::clone(as<NumericVector>(HistIn.slot("Landings")));
  // Hist.slot("Discards") = Rcpp::clone(as<NumericVector>(HistIn.slot("Discards")));
  // Hist.slot("InteractAtAge") =  Rcpp::clone(as<List>(HistIn.slot("InteractAtAge")));
  // Hist.slot("LandingsAtAge") = Rcpp::clone(as<List>(HistIn.slot("LandingsAtAge")));
  // Hist.slot("DiscardsAtAge") = Rcpp::clone(as<List>(HistIn.slot("DiscardsAtAge")));
  // Hist.slot("LandingsAtSize") =Rcpp::clone(as<List>(HistIn.slot("LandingsAtSize")));
  // Hist.slot("DiscardsAtSize") = Rcpp::clone(as<List>(HistIn.slot("DiscardsAtSize")));
  // Hist.slot("Effort") = Rcpp::clone(as<NumericVector>(HistIn.slot("Effort")));
  // Hist.slot("Distribution") = Rcpp::clone(as<NumericVector>(HistIn.slot("Distribution")));
  // Hist.slot("FInteract") = Rcpp::clone(as<NumericVector>(HistIn.slot("FInteract")));
  // Hist.slot("FDead") = Rcpp::clone(as<NumericVector>(HistIn.slot("FDead")));
  // Hist.slot("FRetain") = Rcpp::clone(as<NumericVector>(HistIn.slot("FRetain")));
  // Hist.slot("FInteractArea") = Rcpp::clone(as<List>(HistIn.slot("FInteractArea")));
  // Hist.slot("FDeadArea") = Rcpp::clone(as<List>(HistIn.slot("FDeadArea")));
  // Hist.slot("FRetainArea") = Rcpp::clone(as<List>(HistIn.slot("FRetainArea")));

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
               hv.maxF,
               nStock,
               nFleet,
               nArea);

    if (debug)
      Rcpp::Rcout << "End CalcArea_F \n";

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
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/calc_catch.h
    // ---------------------------------------------------------
  
    if (DoCalcCatch) {
      
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
                hv.WeightFleet,
                nStock,
                nFleet,
                nArea);

      if (debug)
        Rcpp::Rcout << "End CalcCatch \n";
      
    }
    
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
                 hv.InteractAtAge,
                 hv.LandingsAtAge,
                 hv.DiscardsAtAge,
                 hv.Number,
                 nStock,
                 nFleet,
                 nArea
    );
    
    if (debug)
      Rcpp::Rcout << "End CalcOverallF \n";
    
    // Back calculate effort (only needed if maxF constraint is triggered)
    BackCalculateEffort(y,
                        Sims,
                        nSim,
                        hv.FInteract,
                        hv.q,
                        hv.Effort,
                        nStock,
                        nFleet);
    
  
    // maxF constraint now applied within each area
    //
    // // ---------------------------------------------------------
    // // Apply global maxF constraint (on FInteract)
    // // ---------------------------------------------------------
    // 
    // if (debug)
    //   Rcpp::Rcout << "Begin Apply Max F Constraint \n";
    // 
    // ApplyMaxF(y,
    //           Sims,
    //           nSim,
    //           hv.FInteractArea,
    //           hv.FDeadArea,
    //           hv.FRetainArea,
    //           hv.SelAge,
    //           hv.RetAge,
    //           hv.DiscMort,
    //           hv.Distribution,
    //           hv.q,
    //           hv.Effort,
    //           hv.RelSize,
    //           hv.InteractAtAge,
    //           hv.LandingsAtAge,
    //           hv.DiscardsAtAge,
    //           hv.Interactions,
    //           hv.Landings,
    //           hv.Discards,
    //           hv.NaturalMortality,
    //           hv.Number,
    //           hv.WeightFleet,
    //           hv.FInteract,
    //           hv.FDead,
    //           hv.FRetain,
    //           hv.maxF,
    //           nStock,
    //           nFleet,
    //           nArea,
    //           DoCalcCatch,
    //           debug);
    // 
    // if (debug)
    //   Rcpp::Rcout << "End Apply Max F Constraint \n";
    
    if (debug) 
      Rcpp::Rcout << "******End Time Step ****\n\n";
  
    
  }
  return(Hist);
}
