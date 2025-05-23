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
S4 SimulateDynamics_(S4 HistSimIn, 
                       Rcpp::NumericVector TimeSteps
                       ) {
  
  S4 HistSim = clone(HistSimIn);
  S4 OM = HistSim.slot("OM");
  List StockList = OM.slot("Stock");
  List FleetList = OM.slot("Fleet");
  
  NumericVector TimeStepsAll = OM.slot("TimeSteps");
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);
  
  S4 TimeSeries = HistSim.slot("TimeSeries");
  List NumberAtAgeAreaList = TimeSeries.slot("Number"); // nStock
  arma::mat Biomass = TimeSeries.slot("Biomass"); // nStock, nTS
  arma::mat SBiomass = TimeSeries.slot("SBiomass"); // nStock, nTS
  arma::mat SProduction = TimeSeries.slot("SProduction"); // nStock, nTS
  List EffortAreaList = TimeSeries.slot("EffortArea"); // nStock
  arma::cube EffortCube = TimeSeries.slot("Effort"); // nStock, nTS, nFleet
  
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = TimeSeries.slot("FDeadAtAgeArea");
  List FRetainAtAgeAreaList = TimeSeries.slot("FRetainAtAgeArea");
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;
    
    // Rcout << "TSindex = " << TSindex << std::endl;
    
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
    for (int st=0; st<nStock; st++) {
  
      S4 Stock = StockList[st];
      S4 Weight = Stock.slot("Weight");
      S4 Fleet = FleetList[st];

      // Calculate Total Biomass 
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::mat WeightAtAge = Weight.slot("MeanAtAge"); // nAge, nTS
      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
     
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.subcube(arma::span(0, nAge-1), arma::span(TSindex), arma::span(0, nArea-1));
      Biomass.row(st).col(TSindex) = CalcBiomass(NumberAtAgeAreaThisTS, WeightAtAge.col(TSindex));

      // Calculate VBiomass by Area
      // VB = vulnerable (selectivity) x available (spatial closure)
      arma::cube FleetWeightAtAge = Fleet.slot("WeightFleet") ; // nAge, nTS, nFleet
      
   
      
      S4 Selectivity = Fleet.slot("Selectivity");
      S4 Distribution = Fleet.slot("Distribution"); 
      arma::cube SelectivityAtAge = Selectivity.slot("MeanAtAge"); // nAge, nTS, nFleet
      arma::cube ClosureArea = Distribution.slot("Closure"); // nTS, nFleet, nArea
      
      arma::mat VBiomassArea = CalcVBiomass(NumberAtAgeArea.col(TSindex), // nAge, nArea
                                            FleetWeightAtAge.col(TSindex), // nAge, nFleet
                                            SelectivityAtAge.col(TSindex), // nAge, nFleet
                                            ClosureArea.row(TSindex)); // nFleet, nArea
      
      int nFleet = VBiomassArea.n_rows;
      
      // Distribute Effort over Areas
      // currently proportional to VB - ie no SpatTarg
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
      
    
      bool EffortAreaEmpty = all(arma::vectorise(EffortArea.row(TSindex)) < 1E-6);
      if (EffortAreaEmpty) {
        EffortArea.subcube(arma::span(TSindex), arma::span(0, nFleet-1), arma::span(0, nArea-1))= 
          CalcEffortDistribution(VBiomassArea, EffortCube.subcube(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1)), nArea);
  
      }
      EffortAreaList[st] = EffortArea;
      
    
    
      // Calculate F within each Area
   
      S4 FleetEffort = Fleet.slot("Effort");
      arma::mat Catchability = FleetEffort.slot("Catchability"); // nTS, nFleet

      S4 Spatial = Stock.slot("Spatial");
      arma::vec RelativeSize = Spatial.slot("RelativeSize");
 
      S4 Retention = Fleet.slot("Retention");
      arma::cube RetentionAtAge = Retention.slot("MeanAtAge"); // nAge, nTS, nFleet
      S4 DiscardMortality = Fleet.slot("DiscardMortality");
      arma::cube DiscardMortalityAtAge = DiscardMortality.slot("MeanAtAge"); // nAge, nTS, nFleet
        
      List FMortFleetArea = CalcFMortality(EffortArea.row(TSindex), // nFleet, nArea,
                                           Catchability.row(TSindex), // nFleet
                                           RelativeSize, // nArea
                                           SelectivityAtAge.col(TSindex), // nAge, nFleet
                                           RetentionAtAge.col(TSindex), // nAge, nFleet
                                           DiscardMortalityAtAge.col(TSindex), // nAge, nFleet
                                           nArea);
      
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
      FDeadAtAgeAreaStock[TSindex] = FMortFleetArea["FDeadFleetArea"];
      FRetainAtAgeAreaStock[TSindex] = FMortFleetArea["FRetainFleetArea"];
      
      FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
      FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
      
      
      // Calc Spawning Production and Spawning Biomass
      // (first calculate by area and then summed over areas)
      // arma::cube FDeadFleetArea = FMortFleetArea["FDeadFleetArea"];
      // arma::mat FDeadAtAgeArea = arma::sum(FDeadFleetArea,1);
      // arma::vec SProductSBiomass = CalcSpawnProduction(NumberAtAgeArea.col(TSindex), // nAge
      //                                                  FecundityAtAge.col(TSindex), // nAge
      //                                                  MaturityAtAge.col(TSindex), // nAge
      //                                                  WeightAtAge.col(TSindex), // nAge
      //                                                  NaturalMortalityAtAge.col(TSindex), // nAge
      //                                                  FDeadAtAgeArea,
      //                                                  SpawnTimeFrac[st] // double
      // );
      // SProduction.row(st).col(TSindex) = SProductSBiomass[0];
      // SBiomass.row(st).col(TSindex) = SProductSBiomass[1];
    } // end Stock loop
    
  }
 
  
  TimeSeries.slot("Number") = NumberAtAgeAreaList;
  TimeSeries.slot("Biomass") = Biomass;
  TimeSeries.slot("SBiomass") = SBiomass;
  TimeSeries.slot("SProduction") = SProduction;
  TimeSeries.slot("EffortArea") = EffortAreaList;
  TimeSeries.slot("Effort") = EffortCube;
  TimeSeries.slot("FDeadAtAgeArea") = FDeadAtAgeAreaList;
  TimeSeries.slot("FRetainAtAgeArea") = FRetainAtAgeAreaList;
  
  HistSim.slot("TimeSeries") = TimeSeries;
  
  
  
  return(HistSim);
  
 
  
}