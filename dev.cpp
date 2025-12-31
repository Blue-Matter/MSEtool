#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


arma::cube CalcVBiomass_(
    const Rcpp::List& NumberAtAgeAreaList,
    const Rcpp::List& FleetList,
    int TSindex,
    int nStock,
    int nFleet,
    int nArea,
    int debug = 0) {
  if (debug) {
    Rcout << "Start CalcVBiomass\n";
  }
  
  arma::cube VBiomassStockFleetArea(nStock, nFleet, nArea, arma::fill::zeros);
  
  for (int st = 0; st < nStock; ++st) {
    const arma::cube& NumberAtAgeArea = as<arma::cube>(NumberAtAgeAreaList[st]); // nAge x nTS x nArea 
    const Rcpp::S4 Fleet = FleetList[st]; 
    const arma::cube& FleetWeightAtAge = Fleet.slot("WeightFleet"); // nAge x nTS x nFleet
    
    const Rcpp::S4 Selectivity = Fleet.slot("Selectivity"); 
    const arma::cube& SelectivityAtAge = Selectivity.slot("MeanAtAge"); // nAge x nTS x nFleet 
    
    const arma::cube& ClosureArea = Fleet.slot("Closure"); // nTS x nFleet x nArea
    
    const arma::mat Number = NumberAtAgeArea.col(TSindex); // nAge x nArea
    const arma::mat FW     = FleetWeightAtAge.col(TSindex); // nAge x nFleet
    const arma::mat Sel    = SelectivityAtAge.col(TSindex); // nAge x nFleet
    
    for (int fl = 0; fl < nFleet; ++fl) {
      const arma::rowvec closure = ClosureArea.tube(TSindex, fl); // length nArea
      
      if (arma::all(closure == 0.0)) {
        continue;
      }  
      
      const arma::vec age_weight = FW.col(fl) % Sel.col(fl); // nAge
      const arma::vec biomass_by_area =  Number.t() * age_weight; // nArea
      
      for (int area = 0; area < nArea; ++area) {
        const double c = closure(area);
        if (c != 0.0) {
          VBiomassStockFleetArea(st, fl, area) =
            biomass_by_area(area) * c;
        }
      }
    }
  }  
  
  if (debug) {
    Rcout << "End CalcVBiomass\n";
  }  
  
  return VBiomassStockFleetArea;
} 

Rcpp::List DistributeEffort_(  
    Rcpp::List DistributionList,
    arma::cube EffortStockYearFleet,
    const Rcpp::List& NumberAtAgeAreaList,
    const Rcpp::List& FleetList,
    int TSindex,
    int nStock,
    int nFleet,
    int nArea,
    int debug = 0
) {
  if (debug)
    Rcout << "Start DistributeEffort" << std::endl;
  
  // Calculate Vulnerable Biomass for each Fleet & Area 
  
  // VB = vulnerable (selectivity) x available (spatial closure)
  // TODO - selectivity by Area ...
  // TODO - add utility by Stock/Fleet/Age/Area (bio-economic)
  arma::cube VBiomassStockFleetArea = CalcVBiomass_(NumberAtAgeAreaList,
                                                    FleetList,
                                                    TSindex,
                                                    nStock,
                                                    nFleet,
                                                    nArea,
                                                    debug);
  
  // Distribute Effort according to relative VB 
  // TODO - update based on updated CalcVBiomass_ (e.g., CalcUtility ... )
  for (int st = 0; st < nStock; ++st) {
    arma::cube StockEffortDist = DistributionList[st]; // Year, Fleet, Area
    const arma::mat slice = StockEffortDist.tube(TSindex, 0, TSindex, nFleet - 1);
    const bool EffortAreaEmpty = arma::all(arma::all(slice < 1e-6));
    if (EffortAreaEmpty) {  // skip if already populated ...
      arma::vec EffortStock = EffortStockYearFleet.subcube(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1));
      arma::mat EffortFleetArea(nFleet, nArea, arma::fill::zeros);
      
      if (nArea==1) {
        EffortFleetArea(arma::span(0, nFleet-1), 0) = EffortStock;
      } else {
        for (int fl=0; fl<nFleet; fl++) {
          arma::mat VBiomassStockArea = VBiomassStockFleetArea(arma::span(0, nStock-1),
                                                               arma::span(fl),
                                                               arma::span(0, nArea-1));
          arma::rowvec VBiomassArea = arma::sum(VBiomassStockArea, 0);
          arma::rowvec relVBiomassArea = VBiomassArea/arma::accu(VBiomassArea);
          EffortFleetArea.row(fl) = arma::as_scalar(EffortStock(fl)) * relVBiomassArea;
        } 
        StockEffortDist.tube(TSindex, 0, TSindex, nFleet - 1)  = EffortFleetArea;
      } 
    } 
    DistributionList[st] = StockEffortDist;
  } 
  
  if (debug)
    Rcout << "End DistributeEffort" << std::endl;
  
  return DistributionList;
}

// [[Rcpp::export]]
Rcpp::S4 SimulateDynamicsTest(
    Rcpp::S4 HistSim, 
    Rcpp::NumericVector Years,
    int CalcCatch = 1,
    int debug = 0
) {
  // Extract Objects
  Rcpp::S4 OM(HistSim.slot("OM"));
  Rcpp::List StockList(OM.slot("Stock")); // List of `Stock` objects
  Rcpp::List FleetList = OM.slot("Fleet"); // List length `nStock`, each element a `Fleet` object containing info for all fleets
  
  // Time steps
  const NumericVector YearsAll = OM.slot("Years");
  const IntegerVector MatchTS = match(Years, YearsAll);
  
  
  List DistributionList(HistSim.slot("Distribution")); // nStock # effort distribution 
  arma::cube EffortStockYearFleet = as<arma::cube>(HistSim.slot("Effort")); // nStock, nTS, nFleet
  
  const int nStock = StockList.size();
  const int nTS = Years.size();
  const int nFleet = EffortStockYearFleet.n_slices;
  
  List NumberAtAgeAreaList(HistSim.slot("Number")); // nStock
  const arma::cube tmpCube = as<arma::cube>(NumberAtAgeAreaList[0]); // nAge, nTS, nArea
  const int nArea = tmpCube.n_slices;
  
  // Loop over time steps in Years
  for (int timestep=0; timestep<nTS; timestep++) {
    const int TSindex = MatchTS[timestep] -1; // time step index 
    
    if (debug) {
      Rcpp::Rcout << "\n--- Begin Timestep " << Years[timestep] << " ---" << std::endl;
      Rcpp::Rcout << "Timestep Index = " << TSindex << std::endl;
    } 
    
    // Distribute Fishing Effort Over Areas
    DistributionList = DistributeEffort_(
      DistributionList, 
      EffortStockYearFleet,
      NumberAtAgeAreaList,
      FleetList,
      TSindex,
      nStock,
      nFleet,
      nArea,
      debug
    );
    
  } // end of loop over time steps in Years
  
  HistSim.slot("Distribution") = DistributionList;
  return HistSim;
} 







// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
HistSim <- SimList$`1`
HistSim@Effort[1,1,] <- 2
r <- SimulateDynamicsTest(HistSim,  HistYears[1:2]) 


*/
