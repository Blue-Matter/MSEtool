#include <Rcpp.h>

#include "helpers.h"
#include "fishery_sim_state.h"
#include "extract_sim_state.h"
#include "write_sim_state.h"
#include "calc_spatial_effort_dist.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(
    Rcpp::S4 HistIn,
    Rcpp::NumericVector Years,      // Years to loop over
    Rcpp::NumericVector AllYears,   // all Years in Hist
    int nSim,                       // number of simulations
    const int nStock,
    const int nFleet,
    const int nArea,
    const int CalcCatch=1       // calculate catch and overall F?
) {
  
  // Cloone S4 Hist object
  Rcpp::S4 Hist = Rcpp::clone(HistIn);
  
  // time step index 
  std::vector<int> ts_index = CalcTSIndex(Years, AllYears);
  const int nTS = ts_index.size();
  
  if (nSim < 1) {
    Rcpp::stop("nSim must be >= 1");
  }
  
  for (int sim = 0; sim < nSim; ++sim) {
    
    // extract sim-specific values
    FisherySimState st = extract_sim_state(Hist,
                                           sim,
                                           nStock,
                                           nFleet,
                                           nArea
    );
    
    // loop over time steps
    for (int ts = 0; ts < nTS; ++ts) {
      
      const int y = ts_index[ts]; 
      
      // ---------------------------------------------------------
      // MICE Calculations - TODO 
      // 
      //  Note: currently all biological arrays are non-mutable
      //        and no life-history parameters are passed into the 
      //        function.
      //        This will have to be revised once MICE calcs are added
      //
      // ---------------------------------------------------------
      
      
      // -------------------------------------------------
      // Spatial effort distribution
      // -------------------------------------------------
      
      // CalcSpatialEffortDistribution(
      //   y,
      //   st.Distribution,
      //   st.Number,
      //   st.WeightFleet,
      //   st.SelAge,
      //   st.RetAge,
      //   st.q,
      //   st.Closure,
      //   st.Targeting,
      //   st.Effort,
      //   st.RelSize,
      //   nStock,
      //   nFleet,
      //   nArea
      // );

      // -------------------------------------------------
      // Spawning biomass & production
      // -------------------------------------------------
      // CalcSpawnProduction(
      //   y,
      //   st.SBiomass,
      //   st.SProduction,
      //   st.Number,
      //   st.Fecundity,
      //   st.Maturity,
      //   st.Weight,
      //   st.NaturalMortality,
      //   st.SpawnTimeFrac,
      //   st.SPFrom,
      //   st.FDeadArea,
      //   nStock,
      //   nFleet,
      //   nArea
      // );
      
      // -------------------------------------------------
      // Recruitment
      // -------------------------------------------------
       
      // -------------------------------------------------
      // Catch & overall F
      // -------------------------------------------------
       
      // -------------------------------------------------
      // Advance numbers-at-age
      // -------------------------------------------------
      
      
    } // end time step loop
    
    write_sim_state(Hist, st, sim, nStock, nFleet);
    
  } // end sim loop
  
  return(Hist);
}
