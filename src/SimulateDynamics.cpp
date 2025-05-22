#include <RcppArmadillo.h>
#include "check.h"
#include "calculate.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

//' Simulate Fishery Dynamics
 //' 
 //' Calculates the fishery dynamics for a given simulation and the specified
 //' time steps.
 //' 
 //' Optionally can include an MP.
 // [[Rcpp::export]]
List SimulateDynamics_(List NumberAtAgeAreaList, // list by Stock, then array nAge, nTS, nArea
                       List FleetWeightAtAgeList, // list by Stock, then array nAge, nTS, nFleet
                       List SelectivityAtAgeList, // list by Stock, then array nAge, nTS, nFleet  
                       List ClosureAreaList, // list by Stock, then array nTS, nFleet, nArea 
                       List EffortAreaList, // list by Stock, then array nTS, nFleet, nArea
                       arma::cube WeightAtAge, //  nStock, nAge, nTS
                       arma::cube Effort, //  nStock, nTS, nFleet
                       Rcpp::NumericVector TimeSteps,
                       Rcpp::NumericVector TimeStepsAll,
                       NumericVector nAges,
                       int nStock,
                       int nFleet,
                       int nArea
                       ) {
  
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);
  
  // 
  arma::mat Biomass(nStock, nTS);
  
  
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;
    
    // // Do MICE
    //     // update length, weight, fleet weight, natural mortality, maturity, rec pars, etc 
    //     
    //     // Apply MP
    //     // TODO 
    //     // if management interval:
    //     // - generate Data upto TSindex - 1
    //     // - apply MP
    //     // - update selectivity, vulnerability, discard mortality from MP
    //     // - update closed area from MP
    //     // - calculate Effort from TAC (if applicable)
    //     // - apply BioEconomic to Effort
    //     // - return Effort, Selectivity, Vuln, Discard, Closure
    
    // Calculate Total Biomass 
    for (int st=0; st<nStock; st++) {
      int nAge = nAges[st];
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.subcube(arma::span(0, nAge-1), arma::span(TSindex), arma::span(0, nArea-1));
      arma::vec WeightAtAgeThisTS = arma::vectorise(WeightAtAge.subcube(arma::span(st), arma::span(0, nAge-1), arma::span(TSindex)));
      Biomass.row(st).col(TSindex) = CalcBiomass(NumberAtAgeAreaThisTS, WeightAtAgeThisTS);
      
      
      // Calculate VBiomass by Area
      // VB = vulnerable (selectivity) x available (spatial closure)
      arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube ClosureArea = ClosureAreaList[st]; // nTS, nFleet, nArea
      arma::mat VBiomassArea = CalcVBiomass(NumberAtAgeArea.col(TSindex), // nAge, nArea
                                            FleetWeightAtAge.col(TSindex), // nAge, nFleet
                                            SelectivityAtAge.col(TSindex), // nAge, nFleet
                                            ClosureArea.row(TSindex)); // nFleet, nArea
      
      // Distribute Effort over Areas
      // currently proportional to VB - ie no SpatTarg
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
      bool EffortAreaEmpty = all(vectorise(EffortArea.row(TSindex)) < 1E-6);
      if (EffortAreaEmpty) {
        arma::vec FleetEffort = Effort(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1));
        EffortArea.row(TSindex) = CalcEffortDistribution(VBiomassArea, FleetEffort, nArea);
      }
      EffortAreaList[st] = EffortArea;
      
      // Calculate F within each Area
      //     arma::vec FleetCatchability = Catchability(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1));
      //  
      //    // 
      //     List FMortFleetArea = CalcFMortality(EffortArea.row(TSindex), // nFleet, nArea,
      //                                          FleetCatchability, // nFleet
      //                                          RelativeSize.row(st), // nArea
      //                                          SelectivityAtAge.col(TSindex), // nAge, nFleet
      //                                          RetentionAtAge.col(TSindex), // nAge, nFleet
      //                                          DiscardMortalityAtAge.col(TSindex), // nAge, nFleet
      //                                          nArea
      //     );
      
    }
      
      
   
    

    
    // Vulnerable Biomass by Area
    
    
    // Distribute Effort over Areas
    // currently proportional to VB - ie no SpatTarg
    
    
    // Calculate F within each Area
    

  }
  
  
  List Out = List::create(Named("Biomass")=Biomass,
                          Named("EffortAreaList")=EffortAreaList);
  return(Out);
  
}