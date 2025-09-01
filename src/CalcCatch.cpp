#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// [[Rcpp::export]]
S4 CalcCatch_(S4 HistSimIn,
              Rcpp::NumericVector TimeSteps) {
  
 
  S4 HistSim = clone(HistSimIn);
  S4 OM = HistSim.slot("OM");
  List StockList = OM.slot("Stock");
  List FleetList = OM.slot("Fleet");

  // Time Steps
  NumericVector TimeStepsAll = OM.slot("TimeSteps");
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);

  LogicalVector chkTS = any(MatchTS<1);
  if (chkTS[0]) {
    stop("All values in `TimeSteps` must be matched in `OMListSim$TimeSteps`");
  }

  List NumberAtAgeAreaList = HistSim.slot("Number"); // nStock
  List FDeadAtAgeAreaList = HistSim.slot("FDeadAtAgeArea");
  List FRetainAtAgeAreaList = HistSim.slot("FRetainAtAgeArea");

  List RetainAtAgeAreaList = HistSim.slot("Landings");
  List DiscardsAtAgeAreaList = HistSim.slot("Discards");

 int nStock = NumberAtAgeAreaList.size();

  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;

    for (int st=0; st<nStock; st++) {

      // Calculate Discards and Retained Number by Area
      List RetainAtAgeAreaStock = RetainAtAgeAreaList[st];
      List DiscardsAtAgeAreaStock = DiscardsAtAgeAreaList[st];
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];

      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.col(TSindex); // nAge, nArea
      
      arma::cube RetainAtAgeAreaThisTS = RetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube DiscardsAtAgeAreaThisTS = DiscardsAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea


      S4 Stock = StockList[st];
      S4 NaturalMortality = Stock.slot("NaturalMortality");
      arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge"); // nAge, nTS
      
      S4 Fleet = FleetList[st];
      arma::cube FleetWeightAtAge = Fleet.slot("WeightFleet"); // nAge, nTS, nFleet

      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
      int nFleet = RetainAtAgeAreaThisTS.n_cols;

      for (int fl=0; fl<nFleet; fl++) {
        arma::vec fleetweight = FleetWeightAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)); // nAge
        for (int area=0; area<nArea; area++) {
          arma::vec ZmortalityThisArea = arma::sum(FDeadAtAgeAreaThisTS.slice(area),1) + NaturalMortalityAtAge.col(TSindex);
          arma::mat NDeadThisArea = NumberAtAgeAreaThisTS.col(area) % (1-exp(-ZmortalityThisArea)); // nAge, nFleet
          arma::mat FDead = FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area));
          arma::mat FRetain = FRetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area));
          arma::mat RemovalAtAge  = FDead/ZmortalityThisArea % NDeadThisArea % fleetweight;
          arma::mat RetainAtAge  = FRetain/ZmortalityThisArea % NDeadThisArea % fleetweight;
          RetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area)) = RetainAtAge;
          DiscardsAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area)) = RemovalAtAge - RetainAtAge;
        }
      }

      DiscardsAtAgeAreaStock[TSindex] = DiscardsAtAgeAreaThisTS;
      DiscardsAtAgeAreaList[st] = DiscardsAtAgeAreaStock;
      RetainAtAgeAreaStock[TSindex] = RetainAtAgeAreaThisTS;
      RetainAtAgeAreaList[st] = RetainAtAgeAreaStock;
      
    } // end of stock loop
  } // end of time step loop

  
  HistSim.slot("Landings") = RetainAtAgeAreaList;
  HistSim.slot("Discards") = DiscardsAtAgeAreaList;

  return(HistSim);
}