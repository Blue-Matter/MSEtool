#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// for a SINGLE simulation
// [[export]]
List CalcPopDynamics2_(Rcpp::List OMListSim,
                             Rcpp::NumericVector TimeSteps) {


  List OMListSimOut = clone(OMListSim);
  
  
  List AgesList = OMListSimOut["Ages"];

  List Length = OMListSimOut["Length"];
  
  
  List Weight = OMListSimOut["Weight"];
  List WeightMeanAtAge = Weight["MeanAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  
  List Maturity = OMListSimOut["Maturity"];
  
  List Fecundity = OMListSimOut["Fecundity"];
  
  List SRR = OMListSimOut["SRR"];
  
  List SRRPars = SRR["SRRPars"];
  
  List SRRModel = SRR["SRRModel"];
  
  List Spatial = OMListSimOut["Spatial"];
  
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  List BiomassList = OMListSimOut["Biomass"];
  List SProductionList = OMListSimOut["SProduction"];
  List SBiomassList = OMListSimOut["SBiomass"];
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];

  
  List FishingMortality = OMListSimOut["FishingMortality"];
  
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  
  List Effort = OMListSimOut["Effort"];
  List EffortList = Effort["Effort"];
  
  List Selectivity = OMListSimOut["Selectivity"];
  List SelectivityAtAgeList = Selectivity["MeanAtAge"];
  
  List Retention = OMListSimOut["Retention"];
  List RetentionAtAgeList = Retention["MeanAtAge"];
  
  List Distribution = OMListSimOut["Distribution"];
  List ClosureAreaList = Distribution["Closure"];
  
  List EffortAreaList = OMListSimOut["EffortArea"];
  List VBiomassAreaList = OMListSimOut["VBiomassArea"];

  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];

  int nTS = TimeSteps.size();
  int nStock = BiomassList.size();

  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);


    // Do MICE
    // update length, weight, fleet weight, natural mortality, maturity, etc 

    for (int st=0; st<nStock; st++) {
    
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
      arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet 
      arma::cube ClosureArea = ClosureAreaList[st]; // nTS, nFleet, nArea
      
      arma::mat Effort = EffortList[st]; // nTS, nFleet
      
      
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
    
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.col(TSindex); // nAge, nArea
      arma::mat VBiomassAreaThisTS = VBiomassArea.row(TSindex); // nFleet, nArea
      arma::mat FleetWeightAtAgeThisTS = FleetWeightAtAge.col(TSindex); // nAge, nFleet
      arma::mat SelectivityAtAgeThisTS = SelectivityAtAge.col(TSindex); // nAge, nFleet
      arma::mat ClosureAreaThisTS = ClosureArea.row(TSindex); // nFleet, nArea
      
      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
      int nFleet = VBiomassArea.n_cols;

      arma::mat WeightAtAge = WeightMeanAtAge[st];
    
      arma::vec Biomass = BiomassList[st];
    
      // Biomass
      Biomass[TSindex] = arma::accu(WeightAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex)) % arma::sum(NumberAtAgeAreaThisTS,1));
      
      // VBiomass by Area 
      // (assumed vulnerable (selectivity) & available (spatial closure))
      for (int fl=0; fl<nFleet; fl++) {
        for (int area=0; area<nArea; area++) {
          VBiomassAreaThisTS.row(fl).col(area) = arma::accu(NumberAtAgeAreaThisTS.col(area) % FleetWeightAtAgeThisTS.col(fl) % SelectivityAtAgeThisTS.col(fl)) * ClosureAreaThisTS.row(fl).col(area);
        }
      }

      VBiomassArea.row(TSindex) = VBiomassAreaThisTS;
    
    
    // Distribute Effort over Areas
    // (currently proportional to VB)
    // TODO loop
    // for (int fl=0; fl<nFleet; fl++) {
    //   arma::vec vbiomassarea = VBiomassArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1);
    //   double totalVB = arma::accu(vbiomassarea);
    //   arma::vec relvbiomassarea(nArea, arma::fill::zeros);
    //   if (totalVB > 0) {
    //     relvbiomassarea = vbiomassarea/totalVB;
    //   }
    //   
    //   EffortArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = arma::as_scalar(Effort.row(TSindex).col(fl)) * relvbiomassarea; 
    // }

      

      BiomassList[st] = Biomass;
      VBiomassAreaList[st] = VBiomassArea;
      EffortAreaList[st] = EffortArea;
    }
    
    OMListSimOut["Biomass"] = BiomassList;
    OMListSimOut["VBiomassArea"] = VBiomassAreaList;
    OMListSimOut["EffortArea"] = EffortAreaList;

  }

  return(OMListSimOut);
}