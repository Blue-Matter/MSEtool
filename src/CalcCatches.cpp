#include <cpp11.hpp>
#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List CalcCatches_(Rcpp::List OMListSim,
                      Rcpp::NumericVector TimeSteps) {
  
  
  List OMListSimOut = clone(OMListSim);
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List RemovalAtAgeAreaList = OMListSimOut["RemovalAtAgeArea"];
  List RetainAtAgeAreaList = OMListSimOut["RetainAtAgeArea"];
  
  List RemovalNumberAtAgeList = OMListSimOut["RemovalNumberAtAge"];
  List RetainNumberAtAgeList = OMListSimOut["RetainNumberAtAge"];
  List RemovalBiomassAtAgeList =  OMListSimOut["RemovalBiomassAtAge"];
  List RetainBiomassAtAgeList = OMListSimOut["RetainBiomassAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List NaturalMortalityAtAgeList = NaturalMortality["MeanAtAge"];
  
  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];
  
  int nTS = TimeSteps.size();
  int nStock = NumberAtAgeAreaList.size();
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);
    
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
      
      int nArea = NumberAtAgeArea.n_slices;
    
      for (int area=0; area<nArea; area++) {
        arma::vec ZmortalityThisArea = arma::sum(FDeadAtAgeAreaThisTS.slice(area),1) + NaturalMortalityAtAge.col(TSindex);
        arma::mat NDeadThisArea = NumberAtAgeAreaThisTS.col(area) % (1-exp(-ZmortalityThisArea)); // nAge, nFleet
        RemovalAtAgeAreaThisTS.slice(area) = FDeadAtAgeAreaThisTS.slice(area)/ZmortalityThisArea % NDeadThisArea ; // nAge, nFleet;
        RetainAtAgeAreaThisTS.slice(area) = FRetainAtAgeAreaThisTS.slice(area)/ZmortalityThisArea % NDeadThisArea;
      }

      RemovalAtAgeAreaStock[TSindex] = RemovalAtAgeAreaThisTS;
      RemovalAtAgeAreaList[st] = RemovalAtAgeAreaStock;
      
      RetainAtAgeAreaStock[TSindex] = RetainAtAgeAreaThisTS;
      RetainAtAgeAreaList[st] = RetainAtAgeAreaStock;

      // Calculate Aggregate Removals and Retained Number & Biomass
      RemovalNumberAtAge.col(TSindex) = arma::sum(RemovalAtAgeAreaThisTS, 2); // nAge, nFleet
      RetainNumberAtAge.col(TSindex) = arma::sum(RetainAtAgeAreaThisTS, 2); // nAge, nFleet
      RemovalBiomassAtAge.col(TSindex) =  RemovalNumberAtAge.col(TSindex) % FleetWeightAtAge.col(TSindex);
      RetainBiomassAtAge.col(TSindex) =  RetainNumberAtAge.col(TSindex) % FleetWeightAtAge.col(TSindex);

      RemovalAtAgeAreaList[st] = RemovalAtAgeAreaStock; 
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


