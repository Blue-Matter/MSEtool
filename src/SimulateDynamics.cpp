#include <RcppArmadillo.h>
#include "check.h"
#include "calculate.h"
#include "CalcCatch.h"
#include "CalcAggregateF.h"
#include "PopulateNumberNext.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

//' Simulate Fishery Dynamics
//' 
//' Calculates the fishery dynamics for a given simulation and the specified
//' time steps.
//'
// [[Rcpp::export]]
S4 SimulateDynamics_(S4 HistSimIn, 
                       Rcpp::NumericVector TimeSteps,
                       int CalcCatch = 1,
                       int debug = 0) {
  
  S4 HistSim = clone(HistSimIn);
  S4 OM = HistSim.slot("OM");
  List StockList = OM.slot("Stock");
  List FleetList = OM.slot("Fleet");
  
  NumericVector TimeStepsAll = OM.slot("TimeSteps");
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);
  
  List NumberAtAgeAreaList = HistSim.slot("Number"); // nStock
  arma::mat Biomass = HistSim.slot("Biomass"); // nStock, nTS
  arma::mat SBiomass = HistSim.slot("SBiomass"); // nStock, nTS
  arma::mat SProduction = HistSim.slot("SProduction"); // nStock, nTS
  List EffortAreaList = HistSim.slot("EffortArea"); // nStock
  arma::cube EffortCube = HistSim.slot("Effort"); // nStock, nTS, nFleet
  
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = HistSim.slot("FDeadAtAgeArea");
  List FRetainAtAgeAreaList = HistSim.slot("FRetainAtAgeArea");
  
  S4 Unfished = HistSim.slot("Unfished");
  S4 UnfishedEquilibrium = Unfished.slot("Equilibrium");
  arma::mat SP0 = UnfishedEquilibrium.slot("SProduction"); // nStock, nTS
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;

    if (debug) {
      Rcout << "\n\nTimestep = " << TimeSteps[timestep] << std::endl;
      // Rcout << "TimeStepsAll = " << TimeStepsAll << std::endl;
      // Rcout << "MatchTS = " << MatchTS << std::endl;
      // Rcout << "TSmatch = " << TSmatch << std::endl;
      // Rcout << "timestep = " << timestep << std::endl;
      Rcout << "TSindex = " << TSindex << std::endl;
    }
      
    
    // // Do MICE
    //     // update length, weight, fleet weight, natural mortality, maturity, rec pars, etc 
    

    for (int st=0; st<nStock; st++) {
      
      if (debug)
        Rcout << "\nStock = " << st << std::endl;
    
      S4 Stock = StockList[st];
      S4 Weight = Stock.slot("Weight");
      arma::mat WeightAtAge = Weight.slot("MeanAtAge");
      
      S4 Fecundity = Stock.slot("Fecundity");
      arma::mat FecundityAtAge = Fecundity.slot("MeanAtAge");
      
      S4 Maturity = Stock.slot("Maturity");
      arma::mat MaturityAtAge = Maturity.slot("MeanAtAge");
      
      S4 NaturalMortality = Stock.slot("NaturalMortality");
      arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
      
      S4 SRR = Stock.slot("SRR");
      double SpawnTimeFrac = SRR.slot("SpawnTimeFrac");
      
      S4 Spatial = Stock.slot("Spatial");
      arma::vec RelativeSize = Spatial.slot("RelativeSize");
      
      S4 Fleet = FleetList[st];
      
      S4 FleetEffort = Fleet.slot("Effort");
      arma::mat Catchability = FleetEffort.slot("Catchability"); // nTS, nFleet
      
      S4 Selectivity = Fleet.slot("Selectivity");
      arma::cube SelectivityAtAge = Selectivity.slot("MeanAtAge"); // nAge, nTS, nFleet
    
      S4 Retention = Fleet.slot("Retention");
      arma::cube RetentionAtAge = Retention.slot("MeanAtAge"); // nAge, nTS, nFleet
      
      S4 DiscardMortality = Fleet.slot("DiscardMortality");
      arma::cube DiscardMortalityAtAge = DiscardMortality.slot("MeanAtAge"); // nAge, nTS, nFleet
      
      S4 Distribution = Fleet.slot("Distribution"); 
      arma::cube ClosureArea = Distribution.slot("Closure"); // nTS, nFleet, nArea
      
      arma::cube FleetWeightAtAge = Fleet.slot("WeightFleet") ; // nAge, nTS, nFleet

      // Calculate Total Biomass 
      if (debug)
        Rcout << "Total Biomass" << std::endl;
      
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea

      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
    
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.subcube(arma::span(0, nAge-1), arma::span(TSindex), arma::span(0, nArea-1));
      Biomass.row(st).col(TSindex) = CalcBiomass(NumberAtAgeAreaThisTS, WeightAtAge.col(TSindex));

      // Calculate VBiomass by Area
      // VB = vulnerable (selectivity) x available (spatial closure)
      
      if (debug)
        Rcout << "VBiomassArea" << std::endl;
      
      arma::mat VBiomassArea = CalcVBiomass(NumberAtAgeArea.col(TSindex), // nAge, nArea
                                            FleetWeightAtAge.col(TSindex), // nAge, nFleet
                                            SelectivityAtAge.col(TSindex), // nAge, nFleet
                                            ClosureArea.row(TSindex)); // nFleet, nArea
      

      
      // Distribute Effort over Areas
      // currently proportional to VB - ie no SpatTarg
      if (debug)
        Rcout << "Effort" << std::endl;
      
      int nFleet = VBiomassArea.n_rows;
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
      bool EffortAreaEmpty = all(arma::vectorise(EffortArea.row(TSindex)) < 1E-6);
      if (EffortAreaEmpty) {
        EffortArea.subcube(arma::span(TSindex), arma::span(0, nFleet-1), arma::span(0, nArea-1))= 
          CalcEffortDistribution(VBiomassArea, EffortCube.subcube(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1)), nArea);
  
      }
      EffortAreaList[st] = EffortArea;
      
      

      // Calculate F within each Area
      if (debug)
        Rcout << "FMortFleetArea" << std::endl;
      
      List FMortFleetArea = CalcFMortality(EffortArea.row(TSindex), // nFleet, nArea,
                                           arma::vectorise(Catchability.row(TSindex)), // nFleet
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
      if (debug)
        Rcout << "SProductSBiomass" << std::endl;
      
      arma::cube FDeadFleetArea = FMortFleetArea["FDeadFleetArea"];
      arma::mat FDeadAtAgeArea = arma::sum(FDeadFleetArea,1);
      arma::vec SProductSBiomass = CalcSpawnProduction(NumberAtAgeArea.col(TSindex), // nAge
                                                       FecundityAtAge.col(TSindex), // nAge
                                                       MaturityAtAge.col(TSindex), // nAge
                                                       WeightAtAge.col(TSindex), // nAge
                                                       NaturalMortalityAtAge.col(TSindex), // nAge
                                                       FDeadAtAgeArea,
                                                       SpawnTimeFrac // double
      );
      SProduction.row(st).col(TSindex) = SProductSBiomass[0];
      SBiomass.row(st).col(TSindex) = SProductSBiomass[1];
    } // end Stock loop
    
    // Apply SPFrom for spawning production from another stock
    // TODO herm
    if (nStock>1) {
      if (debug)
        Rcout << "SPFrom" << std::endl;
      
      for (int st=0; st<nStock; st++) {
        S4 Stock = StockList[st];
        S4 SRR = Stock.slot("SRR");
        int SPFrom = SRR.slot("SPFrom");
        int fromStock = SPFrom - 1;
        SProduction.row(st).col(TSindex) = SProduction.row(fromStock).col(TSindex);
      }
    }
    
    
    
    // Calculate Recruitment and Numbers at beginning of next time step
    for (int st=0; st<nStock; st++) {
      
      if (debug)
        Rcout << "\n\nCalculate Recruitment and Numbers for Stock " << st << std::endl;
      
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      int nAge = NumberAtAgeArea.n_rows;
      int nTSnumber = NumberAtAgeArea.n_cols;
      int nArea = NumberAtAgeArea.n_slices;

      // Determine Age at Recruitment
      S4 Stock = StockList[st];
      S4 Ages = Stock.slot("Ages");
      bool plusgroup = Ages.slot("PlusGroup");
      arma::vec AgeClasses = Ages.slot("Classes");
      int AgeRec = AgeClasses.min(); // first age class = age of recruitment (should be 0 or 1)
      int TSRec = TSindex + AgeRec; // TSindex + 1 for age-1 recruitment

      S4 Spatial = Stock.slot("Spatial");
      
      if (debug)
        Rcout << "Recruitment " << std::endl;
      
      if (TSRec<nTSnumber) {
        S4 SRR = Stock.slot("SRR");
        arma::vec R0 = SRR.slot("R0");
        arma::vec RecDevHist = SRR.slot("RecDevHist");
        arma::vec RecDevProj = SRR.slot("RecDevProj");
        arma::vec RecDevs = join_cols(RecDevHist, RecDevProj);
        
        Function SRRModel = SRR.slot("Model");
        List SRRPars = SRR.slot("Pars");
        
        // Calculate Recruitment
        // NOTE: uses SP0 and R0 from first time step
        // Uses aggregate SProduction - ie summed over areas
        // TODO option to use time-varying alpha, beta
        
        
        // double SP = arma::as_scalar(SProduction.row(st).col(TSindex));
        // Rcout << "SP = " << SP << std::endl;
        // double r0 =  arma::as_scalar(R0(TSindex));
        // Rcout << "r0 = " << r0 << std::endl;
        // 
        // 
        // double recdev = arma::as_scalar(RecDevs(TSRec));
        // Rcout << "recdev = " << recdev << std::endl;
        
        int sp0_nts = SP0.n_cols;
        double sp0 = arma::as_scalar(SP0.row(st).col(0));
        if (sp0_nts >1 ) {
          sp0 = arma::as_scalar(SP0.row(st).col(TSindex));  
        } 
        // Rcout << "sp0 = " << sp0 << std::endl;
        
        double Recruits = CalcRecruitment_(arma::as_scalar(SProduction.row(st).col(TSindex)),
                                           arma::as_scalar(R0(TSindex)),
                                           sp0,
                                           arma::as_scalar(RecDevs(TSRec)),
                                           SRRModel,
                                           SRRPars,
                                           TSindex);
        if (debug)
          Rcout << "Recruits =  " << Recruits << std::endl;
        
        
        // Distribute Recruits
        if (debug)
          Rcout << "Distribute Recruits " << std::endl;
      
        arma::cube UnfishedDist = Spatial.slot("UnfishedDist"); // nArea, nAge, nTS;
        arma::vec recruitArea(nArea);
        
        for (int area=0; area<nArea; area++) {
          recruitArea(area) = Recruits * arma::as_scalar(UnfishedDist(arma::span(area), arma::span(0), arma::span(TSRec)));
        }
        NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
      }

      if (TSindex <(nTSnumber-1)) {
        
        // Rcout << "timestep = " << timestep << std::endl;
        // Rcout << "TSindex = " << TSindex << std::endl;
        // Rcout << "nTSnumber = " << nTSnumber << std::endl;
        
        List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
        
        S4 NaturalMortality = Stock.slot("NaturalMortality");
        arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
        
        S4 Maturity = Stock.slot("Maturity");
        arma::mat Semelparous = Maturity.slot("Semelparous");
        
        if (debug)
          Rcout << "NumberAtAgeArea Next"  << std::endl;
        
        NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(NumberAtAgeArea.col(TSindex),
                            Semelparous.col(TSindex),
                            FDeadAtAgeAreaStock[TSindex],
                                               NaturalMortalityAtAge.col(TSindex),
                                               plusgroup,
                                               nAge,
                                               nArea);
        
        // Move Population at beginning of next Time Step
        
        if (debug)
          Rcout << "Movement"  << std::endl;
        
        List MovementList = Spatial.slot("Movement");
        NumberAtAgeArea = CalcStockMovement_(NumberAtAgeArea,
                                             MovementList[TSindex+1],
                                                         nAge,
                                                         nArea,
                                                         TSindex+1);
      }
      NumberAtAgeAreaList[st] = NumberAtAgeArea;
      
    } // end of Stock loop

  } // end of Time Step loop
 
  

  
  HistSim.slot("Number") = NumberAtAgeAreaList;
  HistSim.slot("Biomass") = Biomass;
  HistSim.slot("SBiomass") = SBiomass;
  HistSim.slot("SProduction") = SProduction;
  HistSim.slot("EffortArea") = EffortAreaList;
  HistSim.slot("Effort") = EffortCube;
  HistSim.slot("FDeadAtAgeArea") = FDeadAtAgeAreaList;
  HistSim.slot("FRetainAtAgeArea") = FRetainAtAgeAreaList;
  
  // CalcCatch and overall F
  if (CalcCatch>0) {
    HistSim = CalcCatch_(HistSim, TimeSteps);
    HistSim = CalcAggregateF_(HistSim, TimeSteps);
  }
  
  
  return(HistSim);
}