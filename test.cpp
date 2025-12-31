#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

arma::cube CalcVBiomass_(
    const std::vector<arma::cube>& NumberAtAgeArea,
    const std::vector<arma::cube>& FleetWeightAtAge,
    const std::vector<arma::cube>& SelectivityAtAge,
    const std::vector<arma::cube>& ClosureArea,
    int TSindex,
    int debug = 0
) {
  
  if (debug) {
    Rcout << "Start CalcVBiomass\n";
  }
  
  const int nStock = NumberAtAgeArea.size();
  const int nFleet = FleetWeightAtAge[0].n_slices;
  const int nArea  = NumberAtAgeArea[0].n_slices;
  
  arma::cube VBiomass(nStock, nFleet, nArea, arma::fill::zeros);
  
  for (int st = 0; st < nStock; ++st) {
    const arma::mat Number = NumberAtAgeArea[st].col(TSindex);   // nAge × nArea
    const arma::mat FW     = FleetWeightAtAge[st].col(TSindex);  // nAge × nFleet 
    const arma::mat Sel    = SelectivityAtAge[st].col(TSindex); // nAge × nFleet
  
    for (int fl = 0; fl < nFleet; ++fl) {
      const arma::rowvec& closure = ClosureArea[st].tube(TSindex, fl);
      if (arma::accu(closure) == 0.0) continue;

      const arma::vec age_weight = FW.col(fl) % Sel.col(fl);
      const arma::vec biomass_area = Number.t() * age_weight;
      VBiomass.tube(st, fl) = biomass_area.t() % closure;
    }
  } 
  
  if (debug) {
    Rcout << "End CalcVBiomass\n";
  }
  
  return VBiomass;
}



void DistributeEffort_(
    Rcpp::List& DistributionList,
    const arma::cube& EffortStockYearFleet,
    const arma::cube& VBiomass,
    int TSindex,
    int debug=0
) {
  
  if (debug) {
    Rcout << "Start DistributeEffort\n";
  }
  
  const int nStock = DistributionList.size();
  const int nFleet = EffortStockYearFleet.n_slices;
  const int nArea  = VBiomass.n_slices;
  
  // Precompute fleet × area VB
  arma::mat FleetAreaVB(nFleet, nArea, arma::fill::zeros);
  for (int area = 0; area < nArea; ++area) {
    arma::mat slice = VBiomass.slice(area); // nStock × nFleet
    FleetAreaVB.col(area) = arma::sum(slice, 0).t(); // sum over rows (stocks)
  }
   
  for (int st = 0; st < nStock; ++st) {
    arma::cube StockDist = Rcpp::as<arma::cube>(DistributionList[st]);
    auto slice = StockDist.tube(TSindex, 0, TSindex, nFleet - 1);

    if (arma::accu(slice) > 1e-6) continue;

    const arma::vec EffortFleet = EffortStockYearFleet.tube(st, TSindex);
    arma::mat EffortFleetArea(nFleet, nArea, arma::fill::zeros);

    if (nArea == 1) {
      EffortFleetArea.col(0) = EffortFleet;
    } else {
      for (int fl = 0; fl < nFleet; ++fl) {
        double denom = arma::accu(FleetAreaVB.row(fl));
        if (denom > 0.0)
          EffortFleetArea.row(fl) = EffortFleet(fl) * FleetAreaVB.row(fl) / denom;
      }
    }

    StockDist.tube(TSindex, 0, TSindex, nFleet - 1) = EffortFleetArea;
    DistributionList[st] = StockDist;
  }
  
  if (debug) {
    Rcout << "End DistributeEffort\n";
  }
  
};


// [[Rcpp::export]]
Rcpp::S4 SimulateDynamicsTest(
    Rcpp::S4 HistSim,
    Rcpp::NumericVector Years,
    int CalcCatch = 1,
    int debug = 0
) {
  
  
  const S4 OM(HistSim.slot("OM"));
  const NumericVector YearsAll = OM.slot("Years");
  const IntegerVector MatchTS = match(Years, YearsAll);
  
  Rcpp::List DistributionList(HistSim.slot("Distribution"));
  const arma::cube Effort = as<arma::cube>(HistSim.slot("Effort"));

  const List NumberatAgeList(HistSim.slot("Number")); // list nStock
  const List FleetList(OM.slot("Fleet"));

  const int nStock = NumberatAgeList.size();
  
  // ---- Pre-extract all cubes ----
  std::vector<arma::cube> NumberAtAge(nStock); 
  std::vector<arma::cube> WeightFleet(nStock);
  std::vector<arma::cube> SelectivityAtAge(nStock);
  std::vector<arma::cube> Closure(nStock);

  for (int st = 0; st < nStock; ++st) {
    NumberAtAge[st] = as<arma::cube>(NumberatAgeList[st]);

    S4 Fleet = FleetList[st];
    WeightFleet[st] =  Rcpp::as<arma::cube>(Fleet.slot("WeightFleet"));
    Rcpp::S4 Selectivity(Fleet.slot("Selectivity"));
    SelectivityAtAge[st] = Rcpp::as<arma::cube>(Selectivity.slot("MeanAtAge")); // nAge x nTS x nFleet
    Closure[st] =  Rcpp::as<arma::cube>(Fleet.slot("Closure"));
  }
  
  
  // loop over time steps in Years
  for (int timestep = 0; timestep < Years.size(); ++timestep) { 
    const int TSindex = MatchTS[timestep] - 1;
    
    if (debug) {
      Rcpp::Rcout << "\n--- Begin Timestep " << Years[timestep] << " ---" << std::endl;
      Rcpp::Rcout << "Timestep Index = " << TSindex << std::endl;
    } 

    arma::cube VBiomass = CalcVBiomass_(
      NumberAtAge, 
      WeightFleet, 
      SelectivityAtAge, 
      Closure,
      TSindex, 
      debug
    );
    
    DistributeEffort_(DistributionList, Effort, VBiomass, TSindex, debug);
  
  } // end Time Step Loop

  HistSim.slot("Distribution") = DistributionList;
  return HistSim;
}


/*** R
HistSim <- SimList$`1`

HistSim@OM@Fleet$Albacore@Closure |> dim()
HistSim@OM@Fleet$Albacore@Selectivity@MeanAtAge |> dim()


r <- SimulateDynamicsTest(HistSim,  HistYears[1:2], debug=TRUE) 

r@Distribution$Albacore[1:2,, ]


*/
