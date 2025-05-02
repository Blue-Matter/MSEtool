#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;


// [[Rcpp::export]]
List CalcCatch_(Rcpp::List OMListSim,
                Rcpp::NumericVector TimeSteps) {
  
  
  // Check Class
  CheckClass(OMListSim, CharacterVector("OMListSim"), CharacterVector("OMListSim"));
  
  // Clone `OMListSim`
  List OMListSimOut = clone(OMListSim);
  
  // Time Steps
  NumericVector TimeStepsAll = OMListSim["TimeSteps"];
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);
  LogicalVector chkTS = any(MatchTS<1);
  if (chkTS[0]) {
    stop("All values in `TimeSteps` must be matched in `OMListSim$TimeSteps`");
  }
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List RemovalAtAgeAreaList = OMListSimOut["RemovalAtAgeArea"];
  List RetainAtAgeAreaList = OMListSimOut["RetainAtAgeArea"];
  
  List RemovalNumberAtAgeList = OMListSimOut["RemovalNumberAtAge"];
  List RetainNumberAtAgeList = OMListSimOut["RetainNumberAtAge"];
  List RemovalBiomassAtAgeList =  OMListSimOut["RemovalBiomassAtAge"];
  List RetainBiomassAtAgeList = OMListSimOut["RetainBiomassAtAge"];
  
  List NaturalMortalityAtAgeList = OMListSimOut["NaturalMortalityMeanAtAge"];
  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];
  
  int nStock = NumberAtAgeAreaList.size();
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;
    
    for (int st=0; st<nStock; st++) {
      
      // Calculate Removals and Retained Number by Area
      List RemovalAtAgeAreaStock = RemovalAtAgeAreaList[st];
      List RetainAtAgeAreaStock = RetainAtAgeAreaList[st];
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
      
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::cube RemovalAtAgeAreaThisTS = RemovalAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube RetainAtAgeAreaThisTS = RetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      
      arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetainNumberAtAge = RetainNumberAtAgeList[st];  // nAge, nTS, nFleet
      arma::cube RemovalBiomassAtAge = RemovalBiomassAtAgeList[st];  // nAge, nTS, nFleet
      arma::cube RetainBiomassAtAge = RetainBiomassAtAgeList[st];  // nAge, nTS, nFleet
      arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.col(TSindex); // nAge, nArea
      arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
      
      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
      int nFleet = RemovalAtAgeAreaThisTS.n_cols;

      for (int fl=0; fl<nFleet; fl++) {
        for (int area=0; area<nArea; area++) {
          arma::vec ZmortalityThisArea = arma::sum(FDeadAtAgeAreaThisTS.slice(area),1) + NaturalMortalityAtAge.col(TSindex);
          arma::mat NDeadThisArea = NumberAtAgeAreaThisTS.col(area) % (1-exp(-ZmortalityThisArea)); // nAge, nFleet
          arma::mat FDeadAtAgeAreaThisTSFleetArea = FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area));
          arma::mat FRetainAtAgeAreaThisTSFleetArea = FRetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area));
          RemovalAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area)) = FDeadAtAgeAreaThisTSFleetArea/(ZmortalityThisArea % NDeadThisArea);
          RetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl), arma::span(area)) = FRetainAtAgeAreaThisTSFleetArea/(ZmortalityThisArea % NDeadThisArea);
        }
        
        // Calculate Aggregate Removals and Retained Number & Biomass
        arma::vec fleetweight = FleetWeightAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)); // nAge
        arma::vec totalremoval = arma::sum(RemovalAtAgeAreaThisTS.col(fl), 2); // nAge
        arma::vec totalretained = arma::sum(RetainAtAgeAreaThisTS.col(fl), 2); // nAge
        
        RemovalNumberAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)) = totalremoval;
        RetainNumberAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)) = totalretained;
        
        RemovalBiomassAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)) = totalremoval % fleetweight;
        RetainBiomassAtAge(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)) = totalretained % fleetweight;
      }
    
      RemovalAtAgeAreaStock[TSindex] = RemovalAtAgeAreaThisTS;
      RemovalAtAgeAreaList[st] = RemovalAtAgeAreaStock;
      
      RetainAtAgeAreaStock[TSindex] = RetainAtAgeAreaThisTS;
      RetainAtAgeAreaList[st] = RetainAtAgeAreaStock;
      
      RemovalNumberAtAgeList[st] = RemovalNumberAtAge;
      RetainNumberAtAgeList[st] = RetainNumberAtAge;
      RemovalBiomassAtAgeList[st] = RemovalBiomassAtAge;
      RetainBiomassAtAgeList[st] = RetainBiomassAtAge;
      
    } // end of stock loop
  } // end of time step loop 
  
  OMListSimOut["RemovalAtAgeArea"] = RemovalAtAgeAreaList;
  OMListSimOut["RetainAtAgeArea"] = RetainAtAgeAreaList;
  OMListSimOut["RemovalNumberAtAge"] = RemovalNumberAtAgeList;
  OMListSimOut["RetainNumberAtAge"] = RetainNumberAtAgeList;
  OMListSimOut["RemovalBiomassAtAge"] = RemovalBiomassAtAgeList;
  OMListSimOut["RetainBiomassAtAge"] = RetainBiomassAtAgeList;
  
  return(OMListSimOut);
}