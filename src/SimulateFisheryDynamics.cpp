
#include <RcppArmadillo.h>
#include "check.h"
#include "calculate.h"
#include "CalcCatch.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;


// Function f("Range");
// NumericVector test = f(10, 1, 2);
// Rcout << test << std::endl;


//' Simulate Fishery Dynamics
//' 
//' Calculates the fishery dynamics for a given simulation and the specified
//' time steps.
//' 
//' Optionally can include an MP.
// [[Rcpp::export]]
List SimulateFisheryDynamics_(Rcpp::List OMListSim,
                              Rcpp::NumericVector TimeSteps,
                              RObject MP,
                              int CalcCatch = 0) {

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
  
  // Unpack  OMListSimOut
  arma::vec SpawnTimeFrac = OMListSimOut["SpawnTimeFrac"]; // nStock
  arma::vec SPFrom = OMListSimOut["SPFrom"]; // nStock
  
  arma::mat Biomass = OMListSimOut["Biomass"]; // nStock, nTS
  arma::mat R0 = OMListSimOut["R0"]; // nStock, nTS
  arma::mat RecDevs = OMListSimOut["RecDevs"]; 
  arma::mat RelativeSize = OMListSimOut["RelativeSize"]; // nStock, nArea
  arma::mat SBiomass = OMListSimOut["SBiomass"]; // nStock, nTS
  arma::mat SP0 = OMListSimOut["SP0"]; // nStock, nTS
  arma::mat SProduction = OMListSimOut["SProduction"]; // nStock, nTS

  arma::cube Catchability = OMListSimOut["Catchability"]; // nStock, nTS, nFleet
  arma::cube Effort = OMListSimOut["Effort"]; // nStock, nFleet, nTS
 
  List AgesList = OMListSimOut["Ages"];
  List ClosureAreaList = OMListSimOut["Closure"];
  List DiscardMortalityAtAgeList = OMListSimOut["DiscardMortalityMeanAtAge"];
  List EffortAreaList = OMListSimOut["EffortArea"];
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  List FecundityAtAgeList = OMListSimOut["FecundityMeanAtAge"];
  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];
  List MaturityAtAgeList = OMListSimOut["MaturityMeanAtAge"];
  List MovementList = OMListSimOut["Movement"];
  List NaturalMortalityAtAgeList = OMListSimOut["NaturalMortalityMeanAtAge"];
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  List RetentionAtAgeList = OMListSimOut["RetentionMeanAtAge"];
  List SelectivityAtAgeList =  OMListSimOut["SelectivityMeanAtAge"];
  List SemelparousList = OMListSimOut["MaturitySemelparous"];
  List SRRPars = OMListSimOut["SRRPars"];
  List SRRModel = OMListSimOut["SRRModel"];
  List UnfishedDistList = OMListSimOut["UnfishedDist"];
  List VBiomassAreaList = OMListSimOut["VBiomassArea"];
  List WeightAtAgeList = OMListSimOut["WeightMeanAtAge"];
  
  int nStock = Biomass.n_rows;
 
  // Loop Over Time Steps
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = MatchTS[timestep] -1;
    
    // Do MICE
    // update length, weight, fleet weight, natural mortality, maturity, rec pars, etc 
    
    // Apply MP
    // TODO 
    // if management interval:
    // - generate Data upto TSindex - 1
    // - apply MP
    // - update selectivity, vulnerability, discard mortality from MP
    // - update closed area from MP
    // - calculate Effort from TAC (if applicable)
    // - apply BioEconomic to Effort
    // - return Effort, Selectivity, Vuln, Discard, Closure
    
    // Check MP
    int hasMP = CheckMPClass(MP);
    
    // Calculate Biomass & VBiomass, Distribute Effort, Calculate F within Areas,
    // and Calculate SProduction and SBiomass
    for (int st=0; st<nStock; st++) {
    
    // List to matrics and arrays
    arma::mat FecundityAtAge = FecundityAtAgeList[st]; // nAge, nTS
    arma::mat MaturityAtAge = MaturityAtAgeList[st]; // nAge, nTS
    arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
    arma::mat WeightAtAge = WeightAtAgeList[st]; // nAge, nTS
   
    arma::cube ClosureArea = ClosureAreaList[st]; // nTS, nFleet, nArea
    arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // nAge, nTS, nFleet
    arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
    arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
    arma::cube RetentionAtAge = RetentionAtAgeList[st]; // nAge, nTS, nFleet
    arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
    arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea

    List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
    List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
    
    int nFleet = VBiomassArea.n_cols;
    
    // Calculate Total Biomass 
    Biomass.row(st).col(TSindex) = CalcBiomass(NumberAtAgeArea.col_as_mat(TSindex), WeightAtAge.col(TSindex));

    // VBiomass by Area
    // VB = vulnerable (selectivity) x available (spatial closure)
    VBiomassArea.row(TSindex) = CalcVBiomass(NumberAtAgeArea.col(TSindex), // nAge, nArea
                     FleetWeightAtAge.col(TSindex), // nAge, nFleet
                     SelectivityAtAge.col(TSindex), // nAge, nFleet
                     ClosureArea.row(TSindex)); // nFleet, nArea
    VBiomassAreaList[st] = VBiomassArea;
    
    // Distribute Effort over Areas
    // currently proportional to VB - ie no SpatTarg
    bool EffortAreaEmpty = all(vectorise(EffortArea.row(TSindex)) < 1E-6);
    if (EffortAreaEmpty) {
      arma::vec FleetEffort = Effort(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1));
      EffortArea.row(TSindex) = CalcEffortDistribution(VBiomassArea.row(TSindex), FleetEffort);
    } 
    EffortAreaList[st] = EffortArea;

    // Calculate F within each Area
    arma::vec FleetCatchability = Catchability(arma::span(st), arma::span(TSindex), arma::span(0, nFleet-1));
 
    List FMortFleetArea = CalcFMortality(EffortArea.row(TSindex), // nFleet, nArea,
                                         FleetCatchability, // nFleet
                                         RelativeSize.row(st), // nArea
                                         SelectivityAtAge.col(TSindex), // nAge, nFleet
                                         RetentionAtAge.col(TSindex), // nAge, nFleet
                                         DiscardMortalityAtAge.col(TSindex) // nAge, nFleet
    );
    
    FDeadAtAgeAreaStock[TSindex] = FMortFleetArea["FDeadFleetArea"];
    FRetainAtAgeAreaStock[TSindex] = FMortFleetArea["FRetainFleetArea"];
    
    FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
    FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
    
    // Calc Spawning Production and Spawning Biomass
    // (first calculate by area and then summed over areas)
    arma::cube FDeadFleetArea = FMortFleetArea["FDeadFleetArea"];
    arma::mat FDeadAtAgeArea = arma::sum(FDeadFleetArea,1);
    arma::vec SProductSBiomass = CalcSpawnProduction(NumberAtAgeArea.col(TSindex), // nAge
                                                     FecundityAtAge.col(TSindex), // nAge
                                                     MaturityAtAge.col(TSindex), // nAge
                                                     WeightAtAge.col(TSindex), // nAge
                                                     NaturalMortalityAtAge.col(TSindex), // nAge
                                                     FDeadAtAgeArea,
                                                     SpawnTimeFrac[st] // double
    );
    SProduction.row(st).col(TSindex) = SProductSBiomass[0];
    SBiomass.row(st).col(TSindex) = SProductSBiomass[1];
    } // end of Stock loop
    

    // Apply SPFrom for spawning production from another stock 
    // TODO herm
    if (nStock>1) {
      for (int st=0; st<nStock; st++) {
        int fromStock = SPFrom[st] - 1;
        SProduction.row(st).col(TSindex) = SProduction.row(fromStock).col(TSindex);
      }
    } 
    
    // Calculate Recruitment and Numbers at beginning of next time step
    for (int st=0; st<nStock; st++) {
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
      
      // Determine Age at Recruitment
      Rcpp::RObject AgesS4 = AgesList[st];
      bool plusgroup = AgesS4.slot("PlusGroup");
      arma::vec AgeClasses = AgesS4.slot("Classes");
      
      int AgeRec = AgeClasses.min(); // first age class = age of recruitment (should be 0 or 1)
      int TSRec = TSindex + AgeRec; // TSindex + 1 for age-1 recruitment

      if (TSRec<nTS) {
        // Calcate Recruitment
        // NOTE: uses SP0 and R0 from first time step
        // Uses aggregate SProduction - ie summed over areas
        // TODO option to use time-varying alpha, beta
        double Recruits = CalcRecruitment_(arma::as_scalar(SProduction.row(st).col(TSindex)),
                                           arma::as_scalar(R0.row(st).col(TSindex)),
                                           arma::as_scalar(SP0.row(st).col(TSindex)),
                                           arma::as_scalar(RecDevs.row(st).col(TSRec)),
                                           SRRModel[st],
                                           SRRPars[st]);
        
        // Distribute Recruits
        arma::cube UnfishedDist = UnfishedDistList[st]; // nage, nTS, nArea
        arma::vec recruitArea(nArea);
        for (int area=0; area<nArea; area++) {
          recruitArea(area) = Recruits * arma::as_scalar(UnfishedDist(arma::span(0), arma::span(TSRec), arma::span(area)));
        }
        NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
      }
      
      if (timestep<(nTS-1)) {
        List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
        arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
        arma::mat Semelparous = SemelparousList[st]; // nAge, nTS
        
        NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(NumberAtAgeArea.col(TSindex),
                            Semelparous.col(TSindex),
                            FDeadAtAgeAreaStock[TSindex],
                                               NaturalMortalityAtAge.col(TSindex),
                                               plusgroup,
                                               nAge,
                                               nArea);
        
        // Move Population at beginning of next Time Step
        List MovementStock = MovementList[st];
        NumberAtAgeAreaList[st] = CalcStockMovement_(NumberAtAgeArea, MovementStock[TSindex+1], nAge, nArea, TSindex+1);
      } 
      
    } // end of Stock loop 
    
  } // end of Time Step loop
  

  
  // Update OMListSimOut
  // TODO - check all are updated
  OMListSimOut["Biomass"] = Biomass;
  OMListSimOut["EffortArea"] = EffortAreaList;
  OMListSimOut["FDeadAtAgeArea"] = FDeadAtAgeAreaList;
  OMListSimOut["FRetainAtAgeArea"] = FRetainAtAgeAreaList;
  OMListSimOut["FleetWeightAtAge"] = FleetWeightAtAgeList;
  OMListSimOut["NumberAtAgeArea"] = NumberAtAgeAreaList;
  OMListSimOut["SBiomass"] = SBiomass; // nStock, nTS
  OMListSimOut["SProduction"] = SProduction; // nStock, nTS
  OMListSimOut["VBiomassArea"] = VBiomassAreaList;

  
  // CalcCatch and overall F
  if (CalcCatch>0) {
    CalcCatch_(OMListSim, TimeSteps)
    
  }
  
  
  return(OMListSimOut);
}




// [[Rcpp::export]]
List CalcFfromCatch_(arma::vec NumberAtAge, // nAge
                     arma::mat RemovalNAtAge, // nAge, nFleet (number) 
                     arma::mat SelectivityAtAge,  // nAge, nFleet
                     arma::mat RetentionAtAge,  // nAge, nFleet
                     arma::mat DiscardMortalityAtAge,  // nAge, nFleet
                     arma::vec NaturalMortalityAtAge, // nAge
                     int MaxIt=200,   
                     double tolF=1E-4) {
  
  int nAge = RemovalNAtAge.n_rows;
  int nFleet = RemovalNAtAge.n_cols;
  
  arma::vec TotalRemovalsFleet = sum(RemovalNAtAge,0); // nFleet
  
  double TotalNumber = arma::accu(NumberAtAge);
  
  // initial guess at apicalF
  arma::vec apicalF = TotalRemovalsFleet/TotalNumber;
  arma::mat FDeadAtAge(nAge, nFleet);
  arma::mat FRetainAtAge(nAge, nFleet);
  
  for (int i=0; i<MaxIt; i++) {
    // Calculate F-at-age fleet
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FInteract = apicalF(fl) * SelectivityAtAge.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAge.col(fl);
      arma::vec FDiscard = FInteract - FRetain;
      arma::vec FDeadDiscard = FDiscard % DiscardMortalityAtAge.col(fl);
      arma::vec FDead = FDeadDiscard + FRetain;
      FRetainAtAge.col(fl) = FDead;
      FDeadAtAge.col(fl) = FRetain;
    }
    
    arma::vec ZatAge = arma::sum(FDeadAtAge,1) + NaturalMortalityAtAge;
    arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % NumberAtAge;
    
    // derivative of catch wrt ft
    arma::vec dct(nFleet);
    arma::vec predRemovals(nFleet);
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FDead = FDeadAtAge.col(fl);
      predRemovals(fl) = arma::accu(FDead/ZatAge % (1-exp(-ZatAge)) % NumberAtAge);
      arma::vec temp = PopDeadAtAge / ZatAge - ((FDead % PopDeadAtAge / pow(ZatAge,2))) + FDead/ZatAge % exp(-ZatAge)%NumberAtAge;
      dct(fl) = accu(temp);
    }
    
    apicalF = apicalF - (predRemovals - TotalRemovalsFleet)/(0.8*dct);
    NumericVector diff = as<NumericVector>(Rcpp::wrap((predRemovals - TotalRemovalsFleet)));
    NumericVector totalremovals = as<NumericVector>(Rcpp::wrap((TotalRemovalsFleet)));
    
    LogicalVector converge = (Rcpp::abs(diff)/totalremovals) < tolF;
    if (all(converge).is_true())
      break;
  }
  
  
  List L = List::create(Named("FDeadAtAge") = FDeadAtAge,
                        Named("FRetainAtAge") = FRetainAtAge);
  return(L);
  
}

List CalcAggF_(arma::cube FDeadAtAgeAreaThisTS, // nAge, nFleet, nArea
               arma::cube FRetainAtAgeAreaThisTS, // nAge, nFleet, nArea
               arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
               arma::mat RemovalNumberAtAgeThisTS, // nAge, nFleet
               arma::mat RetainNumberAtAgeThisTS, // nAge, nFleet
               arma::mat SelectivityAtAgeThisTS, // nAge, nFleet
               arma::mat RetentionAtAgeThisTS, // nAge, nFleet
               arma::mat DiscardMortalityAtAgeThisTS, // nAge, nFleet
               arma::mat NaturalMortalityAtAgeThisTS) {
  
  int nArea = NumberAtAgeAreaThisTS.n_cols;
  
  if (nArea<2) {
    // no spatial structure
    List L = List::create(Named("FDeadAtAgeThisTS") = FDeadAtAgeAreaThisTS.slice(0),
                          Named("FRetainAtAgeThisTS") = FRetainAtAgeAreaThisTS.slice(0));
    return(L);
  }
  
  // spatial structure - need to calculate overall 
  int nAge = RetainNumberAtAgeThisTS.n_rows;
  int nFleet = RetainNumberAtAgeThisTS.n_cols;
  
  arma::vec NumberAtAge = arma::sum(NumberAtAgeAreaThisTS,1); // summed over areas
  
  arma::vec TotalRemovalsFleet = sum(RemovalNumberAtAgeThisTS,0); // nFleet
  
  LogicalVector ZeroCatch(nFleet);
  ZeroCatch = TotalRemovalsFleet < 1E-4;
  
  if ((all(ZeroCatch).is_true())) {
    // no catches for any fleets
    // return F=0
    arma::mat FZeros(nAge, nFleet);
    
    List L = List::create(Named("FDeadAtAgeThisTS") = FZeros,
                          Named("FRetainAtAgeThisTS") = FZeros);
    return(L);
  }
  
  // check if F same in all areas
  LogicalVector IdenticalF(nFleet);
  for (int fl=0; fl<nFleet; fl++) {
    arma::mat fdeadfleet = FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl,fl), arma::span(0,nArea-1));
    NumericMatrix FDeadFleet = as<NumericMatrix>(Rcpp::wrap(fdeadfleet));
    int same = 0;
    for (int area=1; area<nArea; area++) {
      NumericVector sameAge = FDeadFleet(_,0) / FDeadFleet(_,area);
      same += sum(sameAge);
    }
    if (same == nAge) {
      IdenticalF(fl) = TRUE;
    }
  }
  
  if ((all(IdenticalF).is_true())) {
    // identical F across areas for all fleets
    List L = List::create(Named("FDeadAtAgeThisTS") = FDeadAtAgeAreaThisTS.slice(0),
                          Named("FRetainAtAgeThisTS") = FRetainAtAgeAreaThisTS.slice(0));
    return(L);
  }
  
  
  // Solve for overall F given overall Catch (Number) and Numbers
  List Foverall = CalcFfromCatch_(NumberAtAge,
                                  RemovalNumberAtAgeThisTS,
                                  SelectivityAtAgeThisTS,
                                  RetentionAtAgeThisTS,
                                  DiscardMortalityAtAgeThisTS,
                                  NaturalMortalityAtAgeThisTS
  );
  
  List L = List::create(Named("FDeadAtAgeThisTS") = Foverall["FDeadAtAge"],
                        Named("FRetainAtAgeThisTS") = Foverall["FRetainAtAge"]);
  return(L);
  
}

// [[Rcpp::export]]
List CalcAggregateF_(Rcpp::List OMListSim,
                     Rcpp::NumericVector TimeSteps) {
  
  List OMListSimOut = clone(OMListSim);
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];
  int nTS = TimeSteps.size();
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List FDeadAtAgeList = OMListSimOut["FDeadAtAge"];
  List FRetainAtAgeList = OMListSimOut["FRetainAtAge"];
  
  List RemovalNumberAtAgeList = OMListSimOut["RemovalNumberAtAge"];
  List RetainNumberAtAgeList = OMListSimOut["RetainNumberAtAge"]; 
  
  List Selectivity = OMListSimOut["Selectivity"];
  List SelectivityAtAgeList = Selectivity["MeanAtAge"];
  
  List Retention = OMListSimOut["Retention"];
  List RetentionAtAgeList = Retention["MeanAtAge"];
  
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  List DiscardMortalityAtAgeList = DiscardMortality["MeanAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List NaturalMortalityAtAgeList = NaturalMortality["MeanAtAge"];
  
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);
    
    for (int st=0; st<nStock; st++) {
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
      
      arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      
      arma::cube FDeadAtAge = FDeadAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube FRetainAtAge = FRetainAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      
      arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetainNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetentionAtAge = RetentionAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
      
      // Calculate aggregate F over all areas
      List AggF = CalcAggF_(FDeadAtAgeAreaThisTS,
                            FRetainAtAgeAreaThisTS,
                            NumberAtAgeArea.col(TSindex),
                            RemovalNumberAtAge.col(TSindex),
                            RetainNumberAtAge.col(TSindex),
                            SelectivityAtAge.col(TSindex),
                            RetentionAtAge.col(TSindex),
                            DiscardMortalityAtAge.col(TSindex),
                            NaturalMortalityAtAge.col(TSindex)
      );
      
      arma::mat FDeadAtAgeThisTS = AggF["FDeadAtAgeThisTS"];
      arma::mat FRetainAtAgeThisTS = AggF["FRetainAtAgeThisTS"];
      
      FDeadAtAge.col(TSindex) = FDeadAtAgeThisTS;
      FRetainAtAge.col(TSindex) = FRetainAtAgeThisTS;
      
      FDeadAtAgeList[st] = FDeadAtAge;
      FRetainAtAgeList[st] = FRetainAtAge;
    } // end of stock loop
    
  } // end of time step loop
  
  OMListSim["FDeadAtAge"] = FDeadAtAgeList;
  OMListSim["FRetainAtAge"] = FRetainAtAgeList;
  
  return(OMListSim);
}



