
#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

void CheckClass(RObject object,
                 Rcpp::CharacterVector reqClass,
                 Rcpp::CharacterVector argName) {

  bool b = object.hasAttribute("class");
  int counter = 0;
  int nClass = reqClass.size();
  if (b) {
    CharacterVector actualClass = object.attr("class");
    for(int i = 0; i < nClass; i++) {
      if (actualClass[0] == reqClass[i])
        counter++;
    }
  }

  if (!b || counter ==0)
    stop("argument %s must be class %s", argName, reqClass);

}

void CheckDimensions(int req, 
                     int act,
                     Rcpp::CharacterVector object,
                     Rcpp::CharacterVector name,
                     int type =0) {
  if (act != req) {
    Rcpp::Rcout << "Required: " << req << std::endl;
    Rcpp::Rcout << "Provided: " << act << std::endl;
    if (type == 0)
      stop("%s must have %s rows", object, name);
    if (type == 1)
      stop("%s must have %s columns", object, name);
    if (type == 3)
      stop("%s must have %s slices (3rd dimension)", object, name);
  }
}

void CheckLength(int req, 
                 int act,
                 Rcpp::CharacterVector object,
                 Rcpp::CharacterVector name) {
  if (act != req) {
    Rcpp::Rcout << "Required: " << req << std::endl;
    Rcpp::Rcout << "Provided: " << act << std::endl;
    stop("%s must be length %s ", object, name);
  }
}

// TODO - doesn't handle MP Lists
int CheckMPClass(RObject MP) {
  int hasMP = 0;
  if(is<Function>(MP)){
    // check the class
    CharacterVector req = {"MP", "MMP"};
    CheckClass(MP, req, CharacterVector("MP"));
    hasMP = 1;
  } else if(!MP.isNULL()){
    stop("Argument `MP` must be either NULL or a function class `MP` or `MMP`");
  }
  return(hasMP);
}


double CalcBiomass(arma::mat NumberAtAgeArea, // nAge, nArea
                   arma::vec WeightAtAge){ // nAge
  
  double B = arma::accu(WeightAtAge %  arma::sum(NumberAtAgeArea,1));
  return(B);
}

//' Calculates Vulnerable Biomass by Fleet and Area
//' 
//' Vulnerable biomass is defined as the selected age-classes in areas that are
//' open to fishing.
//' 
//' All matrices must have the correct dimensions:
//' - `nAge` is calculated from `nrow(NumberAtAgeArea)`
//' - `nArea` is calculated from `ncol(NumberAtAgeArea)`
//' - `nFleet` is calculated from `ncol(FleetWeightAtAgeFleet)`
//'
//' @param NumberAtAgeArea Numeric matrix of number-at-age with `nAge` rows and `nArea` columns
//' @param FleetWeightAtAgeFleet Numeric matrix of mean weight-at-age with `nAge` rows and `nFleet` columns
//' @param SelectivityAtAgeFleet Numeric matrix of selectivity-at-age with `nAge` rows and `nFleet` columns
//' @param ClosureFleetArea Boolean matrix of area closures `nFleet` rows and `nArea` columns; 0 for closed, 1 for open
//' 
//' @return Numeric matrix of vulnerable biomass with `nFleet` rows and `nArea` columns
// [[Rcpp::export]]
arma::mat CalcVBiomass(arma::mat NumberAtAgeArea, // nAge, nArea
                        arma::mat FleetWeightAtAgeFleet, // nAge, nFleet
                        arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                        arma::mat ClosureFleetArea) { // nFleet, nArea) 

  int nAge = NumberAtAgeArea.n_rows;
  int nArea = NumberAtAgeArea.n_cols;
  int nFleet = FleetWeightAtAgeFleet.n_cols;

  CheckDimensions(nAge, FleetWeightAtAgeFleet.n_rows, "FleetWeightAtAgeFleet", "nAge");
  CheckDimensions(nAge, SelectivityAtAgeFleet.n_rows, "SelectivityAtAgeFleet", "nAge");
  CheckDimensions(nFleet, SelectivityAtAgeFleet.n_cols, "SelectivityAtAgeFleet", "nFleet", 1);
  CheckDimensions(nFleet, ClosureFleetArea.n_rows, "ClosureFleetArea", "nFleet");
  CheckDimensions(nArea, ClosureFleetArea.n_cols, "ClosureFleetArea", "nArea", 1);
  
  arma::mat VBiomassFleetArea(nFleet, nArea, arma::fill::zeros);
  for (int area=0; area<nArea; area++) {
    for (int fl=0; fl<nFleet; fl++) {
      VBiomassFleetArea.row(fl).col(area) += arma::sum(NumberAtAgeArea.col(area) %
        SelectivityAtAgeFleet.col(fl) %
        FleetWeightAtAgeFleet.col(fl),0) *
        arma::as_scalar(ClosureFleetArea.row(fl).col(area));
    }

  }
  return(VBiomassFleetArea);
}


//' Distribute Fishing Effort Over Areas
//' 
//' Currently fishing effort is distributed across areas proportional to vulnerable biomass
//' 
//' @param VBiomassFleetArea Numeric matrix of vulnerable biomass with `nFleet` rows and `nArea` columns
//' @param Effort Numeric vector of length `nFleet` with total effort for each fleet
//' 
//' @return Numeric matrix of fishing effort with `nFleet` rows and `nArea` columns
// [[Rcpp::export]]
arma::mat DistributeEffort(arma::mat VBiomassFleetArea, // nFleet, nArea
                           arma::vec Effort) {  // nFleet
  
  
  int nFleet = VBiomassFleetArea.n_rows;
  int nArea = VBiomassFleetArea.n_cols;
  
  CheckLength(Effort.size(), nFleet, "Effort", "nFleet (nrow(VBiomassFleetArea))");
  
  arma::mat EffortFleetArea(nFleet, nArea, arma::fill::zeros);
  
  for (int fl=0; fl<nFleet; fl++) {
    arma::rowvec relvbiomassarea(nArea, arma::fill::zeros);
    double totalVB = arma::accu(VBiomassFleetArea.row(fl));
    if (totalVB > 0) {
        relvbiomassarea = VBiomassFleetArea.row(fl)/totalVB;
    }
    EffortFleetArea.row(fl) = arma::as_scalar(Effort(fl)) * relvbiomassarea;
  }
  return(EffortFleetArea);
}

//' Calculate Fishing Mortality from Effort and Catchability for each Fleet and Area
//' 
// [[Rcpp::export]]
List CalcFMortality(arma::mat EffortFleetArea, // nFleet, nArea
                    arma::vec Catchability, // nFleet
                    arma::rowvec RelativeSize, // nArea
                    arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                    arma::mat RetentionAtAgeFleet, // nAge, nFleet
                    arma::mat DiscardMortalityAtAgeFleet // nAge, nFleet
                      ) {
  
  int nFleet = EffortFleetArea.n_rows;
  int nArea = EffortFleetArea.n_cols;
  int nAge = SelectivityAtAgeFleet.n_rows;
  
  CheckLength(Catchability.size(), nFleet, "Catchability", "nFleet (nrow(EffortFleetArea))");
  CheckLength(RelativeSize.size(), nArea, "RelativeSize", "nArea (ncol(EffortFleetArea))");
  
  CheckDimensions(nFleet, SelectivityAtAgeFleet.n_cols, "SelectivityAtAgeFleet", "nFleet", 1);
  CheckDimensions(nAge, RetentionAtAgeFleet.n_rows, "RetentionAtAgeFleet", "nAge");
  CheckDimensions(nFleet, RetentionAtAgeFleet.n_cols, "RetentionAtAgeFleet", "nFleet", 1);
  CheckDimensions(nAge, DiscardMortalityAtAgeFleet.n_rows, "DiscardMortalityAtAgeFleet", "nAge");
  CheckDimensions(nFleet, DiscardMortalityAtAgeFleet.n_cols, "DiscardMortalityAtAgeFleet", "nFleet", 1);
  
  arma::cube FDeadFleetArea(nAge, nFleet, nArea);
  arma::cube FRetainFleetArea(nAge, nFleet, nArea);
  
  for (int fl=0; fl<nFleet; fl++) {
    for (int area=0; area<nArea; area++) {
      double catchabilityArea = arma::as_scalar(Catchability(fl)/RelativeSize(area));
      double effortarea = arma::as_scalar(EffortFleetArea.row(fl).col(area));
      arma::vec FInteract = effortarea * catchabilityArea * SelectivityAtAgeFleet.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAgeFleet.col(fl);
      arma::vec Discard = FInteract - FRetain;
      arma::vec DeadDiscard = Discard % DiscardMortalityAtAgeFleet.col(fl);
      FRetainFleetArea.subcube(0, fl, area, nAge-1, fl, area) = FRetain;
      FDeadFleetArea.subcube(0, fl, area, nAge-1,fl, area) = FRetain + DeadDiscard;
    }
  }

  List L = List::create(Named("FDeadFleetArea") = FDeadFleetArea,
                        Named("FRetainFleetArea") = FRetainFleetArea);
  return(L);
}

//' Calculate Spawning Production and Spawning Biomass 
//' 
//' SPFrom is handled later
// [[Rcpp::export]]
arma::vec CalcSpawnProduction(arma::mat NumberAtAgeArea, // nAge, nArea
                              arma::vec FecundityAtAge, // nAge
                              arma::vec MaturityAtAge, // nAge,
                              arma::vec WeightAtAge, // nAge
                              arma::vec NaturalMortalityAtAge, // nAge
                              arma::mat FDeadAtAgeArea, // nAge, nArea
                              double SpawnTimeFrac= 0) {
  
  int nAge = NumberAtAgeArea.n_rows;
  int nArea = NumberAtAgeArea.n_cols;
  
  CheckLength(FecundityAtAge.size(), nAge, "FecundityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(MaturityAtAge.size(), nAge, "MaturityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(WeightAtAge.size(), nAge, "WeightAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(NaturalMortalityAtAge.size(), nAge, "NaturalMortalityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckDimensions(nAge, FDeadAtAgeArea.n_rows, "FDeadAtAgeArea", "nAge");
  CheckDimensions(nArea, FDeadAtAgeArea.n_cols, "FDeadAtAgeArea", "nArea", 1);
  
  arma::mat SProductionArea(nAge, nArea, arma::fill::zeros); // nAge, nArea
  arma::mat SBiomassArea(nAge, nArea, arma::fill::zeros); // nAge, nArea
  
  for (int area=0; area<nArea; area++) {
    arma::vec NSpawnThisArea = NumberAtAgeArea.col(area);
    
    if (SpawnTimeFrac > 0) {
      arma::vec FDeadThisArea = FDeadAtAgeArea.col(area); // nAge
      arma::vec SpawnMortality = (NaturalMortalityAtAge + FDeadThisArea) * SpawnTimeFrac;
      NSpawnThisArea = NSpawnThisArea % exp(-SpawnMortality);
    }
    SProductionArea.col(area) = NSpawnThisArea % FecundityAtAge;
    SBiomassArea.col(area) = NSpawnThisArea % WeightAtAge % MaturityAtAge; 
  }
  
  arma::vec out(2, arma::fill::zeros);
  out[0] = arma::accu(SProductionArea);
  out[1] = arma::accu(SBiomassArea);
  return(out);
}


double CalcRecruitment_(double SProduction,
                        double R0,
                        double SP0,
                        double RecDev,
                        Function SRRModel,
                        List SRRPars) {
  
  // NOTE: uses SP0 and R0 from first time step
  // TODO option to use time-varying alpha, beta
  List Arglist = List::create(Named("S") = SProduction,
                              Named("S0") = SP0,
                              Named("R0") = R0);
  
  CharacterVector ParNames = SRRPars.names();
  CharacterVector ArglistNames(3+SRRPars.size());
  ArglistNames[0] = "S";
  ArglistNames[1] = "S0";
  ArglistNames[2] = "R0";
  
  for (int i=0; i<SRRPars.size(); i++) {
    NumericVector argVec = SRRPars[i];
    double arg = argVec[i];
    Arglist.push_back(arg);
    ArglistNames[3+i] = ParNames[i];
  }
  Arglist.attr("names") = ArglistNames;
  
  Rcpp::Environment base("package:base");
  Rcpp::Function doCall = base["do.call"];
  
  RObject RecruitsEQ = doCall(SRRModel, Arglist);
  // TODO - check for valid value
  double Recruits = as<double>(RecruitsEQ) * RecDev;
  return(Recruits);
}


arma::mat CalcNumberNext_(arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
                           arma::vec Semelparous,  // nAge
                           arma::cube FDeadAtAgeAreaThisTS,  // nAge, nFleet, nArea
                           arma::vec NaturalMortalityAtAgeThisTS,
                           bool plusgroup,
                           int nAge,
                           int nArea) {
  
  arma::mat Nnext(nAge, nArea, arma::fill::zeros);
  
  arma::mat FDead = arma::sum(FDeadAtAgeAreaThisTS, 1);
  arma::mat Zmortality = FDead.each_col() + NaturalMortalityAtAgeThisTS;
  
  for (int age=0; age<(nAge-1); age++) {
    Nnext.row(age+1) = NumberAtAgeAreaThisTS.row(age) % exp(-Zmortality.row(age)) * (1-Semelparous(age));
  }
  
  if (plusgroup) {
    Nnext.row(nAge-1) = Nnext.row(nAge-1) + (NumberAtAgeAreaThisTS.row(nAge-1) % exp(-Zmortality.row(nAge-1)) * (1-Semelparous(nAge-1)));
  }
  
  return(Nnext);
}



arma::cube MoveStock_(arma::cube NumberAtAgeArea,
                      arma::cube Movement, // nAge, FromArea, ToArea
                      int nAge,
                      int nArea,
                      int TSindex) {
  
  arma::mat NumberAtAgeAreaThisTS =  NumberAtAgeArea.col(TSindex); // nAge, nArea
  arma::mat NumberAtAgeAreaMoved(nAge, nArea, arma::fill::zeros);  // nAge, nArea
  
  for (int age=0; age<nAge; age++) {
    arma::mat moveage = Movement.row(age);
    for (int toArea=0; toArea<nArea; toArea++) {
      arma::vec Narea(nArea, arma::fill::zeros);
      for (int fromArea=0; fromArea<nArea; fromArea++) {
        Narea(toArea) += NumberAtAgeAreaThisTS(age, fromArea) * moveage(fromArea, toArea);
      }
      NumberAtAgeAreaMoved(age, toArea) = Narea(toArea);
    }
  }
  
  NumberAtAgeArea.col(TSindex) = NumberAtAgeAreaMoved;
  return(NumberAtAgeArea);
};



// Function f("Range");
// NumericVector test = f(10, 1, 2);
// Rcout << test << std::endl;



//' Calculate Fishery Dynamics
//' 
//' Calculates the fishery dynamics for a given simulation and the specified
//' time steps.
//' 
//' Optionally can include an MP.
// [[Rcpp::export]]
List CalcFisheryDynamics_(Rcpp::List OMListSim,
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
  arma::mat RelativeSize = OMListSimOut["RelativeSize"]; // nStock, nArea
  arma::mat SBiomass = OMListSimOut["SBiomass"]; // nStock, nTS
  arma::mat SProduction = OMListSimOut["SProduction"]; // nStock, nTS

  arma::cube Catchability = OMListSimOut["Catchability"]; // nStock, nFleet, nTS
  arma::cube Effort = OMListSimOut["Effort"]; // nStock, nFleet, nTS
 
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
      EffortArea.row(TSindex) = DistributeEffort(VBiomassArea.row(TSindex), FleetEffort);
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
        int fromStock = SPFrom[st];
        SProduction.row(st).col(TSindex) = SProduction.row(fromStock).col(TSindex);
      }
    } 
    
    // Calculate Recruitment and Numbers at beginning of next time step
    
    
    
    // CalcCatch and overall F
    if (CalcCatch>0) {
      
      
    }
    
    
  } // end of Time Step loop
  
  // Update OMListSimOut
  // TODO - check all are updated
  OMListSimOut["Biomass"] = Biomass;
  OMListSimOut["EffortArea"] = EffortAreaList;
  OMListSimOut["FDeadAtAgeArea"] = FDeadAtAgeAreaList
  OMListSimOut["FRetainAtAgeArea"] = FRetainAtAgeAreaList;
  OMListSimOut["FleetWeightAtAge"] = FleetWeightAtAgeList;
  OMListSimOut["NumberAtAgeArea"] = NumberAtAgeAreaList;
  OMListSimOut["SBiomass"] = SBiomass; // nStock, nTS
  OMListSimOut["SProduction"] = SProduction; // nStock, nTS
  OMListSimOut["VBiomassArea"] = VBiomassAreaList;


   
   
    
    
   
    
    
    
 

  
  
  return(OMListSimOut);
}



// [[Rcpp::export]]
List CalcPopDynamics_(Rcpp::List OMListSim,
                      Rcpp::NumericVector TimeSteps) {

  List OMListSimOut = clone(OMListSim);
  
  List AgesList = OMListSimOut["Ages"];
  List Length = OMListSimOut["Length"];
  
  List Weight = OMListSimOut["Weight"];
  List WeightAtAgeList = Weight["MeanAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List NaturalMortalityAtAgeList = NaturalMortality["MeanAtAge"];
  
  List Maturity = OMListSimOut["Maturity"];
  List MaturityAtAgeList = Maturity["MeanAtAge"];
  List SemelparousList = Maturity["Semelparous"];
          
  List Fecundity = OMListSimOut["Fecundity"];
  List FecundityAtAgeList = Fecundity["MeanAtAge"];
  
  List SRR = OMListSimOut["SRR"];
  arma::vec SpawnTimeFrac = SRR["SpawnTimeFrac"];
  List SPFromList = SRR["SPFrom"];
  List R0List = SRR["R0"];
  
  List SP0List = OMListSimOut["SP0"];
  List RecDevsList = SRR["RecDevs"];
  List SRRParsList = SRR["SRRPars"];
  List SRRModelList = SRR["SRRModel"];
  
  List Spatial = OMListSimOut["Spatial"];
  List UnfishedDistList = Spatial["UnfishedDist"];
  List RelativeSizeList = Spatial["RelativeSize"];
  List MovementList = Spatial["Movement"];
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  List BiomassList = OMListSimOut["Biomass"];
  List SProductionList = OMListSimOut["SProduction"];
  List SBiomassList = OMListSimOut["SBiomass"];
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];

  List FishingMortality = OMListSimOut["FishingMortality"];
  
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  List DiscardMortalityAtAgeList = DiscardMortality["MeanAtAge"];
  
  arma::cube Effort = OMListSimOut["Effort"]; // nStock, nFleet, nTS
  arma::cube Catchability = OMListSimOut["Catchability"]; // nStock, nFleet, nTS
  
  List Selectivity = OMListSimOut["Selectivity"];
  List SelectivityAtAgeList = Selectivity["MeanAtAge"];
  
  List Retention = OMListSimOut["Retention"];
  List RetentionAtAgeList = Retention["MeanAtAge"];
  
  List Distribution = OMListSimOut["Distribution"];
  List ClosureAreaList = Distribution["Closure"];
  
  List EffortAreaList = OMListSimOut["EffortArea"];
  List VBiomassAreaList = OMListSimOut["VBiomassArea"];
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];

  int nTS = TimeSteps.size();
  int nStock = BiomassList.size();

  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);

    // Do MICE
    // update length, weight, fleet weight, natural mortality, maturity, etc 

    for (int st=0; st<nStock; st++) {
      
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
    
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
      arma::cube ClosureArea = ClosureAreaList[st]; // nTS, nFleet, nArea
      arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetentionAtAge = RetentionAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea

      
      // arma::mat Effort = EffortList[st]; // nTS, nFleet
      //arma::mat Catchability = CatchabilityList[st]; // nTS, nFleet
      arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
      arma::mat WeightAtAge = WeightAtAgeList[st]; // nAge, nTS
      arma::mat FecundityAtAge = FecundityAtAgeList[st]; // nAge, nTS
      arma::mat MaturityAtAge = MaturityAtAgeList[st]; // nAge, nTS
      
      arma::vec Biomass = BiomassList[st];
      arma::vec SProduction = SProductionList[st]; // nTS
      arma::vec SBiomass = SBiomassList[st]; // nTS
      
      // int nAge = NumberAtAgeArea.n_rows;
      // int nArea = NumberAtAgeArea.n_slices;
      int nFleet = VBiomassArea.n_cols;
      
      // Biomass
      Biomass[TSindex] = arma::accu(WeightAtAge.col(TSindex) % arma::sum(NumberAtAgeArea.col_as_mat(TSindex),1));
      BiomassList[st] = Biomass;
      
      // VBiomass by Area
      // (assumed vulnerable (selectivity) & available (spatial closure))
      VBiomassArea.row(TSindex) = CalcVBiomass(NumberAtAgeArea.col(TSindex), // nAge, nArea
                                                FleetWeightAtAge.col(TSindex), // nAge, nFleet
                                                SelectivityAtAge.col(TSindex), // nAge, nFleet
                                                ClosureArea.row(TSindex)); // nFleet, nArea
      VBiomassAreaList[st] = VBiomassArea;

      // Distribute Effort over Areas
      // (currently proportional to VB) - currently no SpatTarg
      Rcout << "Here\n";
      bool EffortAreaEmpty = all(vectorise(EffortArea.row(TSindex)) < 1E-6);
      if (EffortAreaEmpty) {
        Rcout << "Here\n";
        arma::rowvec FleetEffort = Effort(arma::span(st), arma::span(0, nFleet-1), arma::span(TSindex));
  
        Rcout << "Herio \n";
        EffortArea.row(TSindex) = DistributeEffort(VBiomassArea.row(TSindex), FleetEffort);
      } 
      EffortAreaList[st] = EffortArea;
      Rcout << "Here 2\n";

      // // Calculate F within each Area
      // arma::rowvec FleetCatchability = Catchability.subcube(st, 0, TSindex, st, nFleet-1, TSindex);
      // List FMortFleetArea = CalcFMortality(EffortArea.row(TSindex), // nFleet, nArea,
      //                                      FleetCatchability, // nFleet
      //                                      RelativeSizeList[st], // nArea
      //                                      SelectivityAtAge.col(TSindex), // nAge, nFleet
      //                                      RetentionAtAge.col(TSindex), // nAge, nFleet
      //                                      DiscardMortalityAtAge.col(TSindex) // nAge, nFleet
      //                                      );
      // Rcout << "Here 3\n";
      // FDeadAtAgeAreaStock[TSindex] = FMortFleetArea["FDeadFleetArea"];
      // FRetainAtAgeAreaStock[TSindex] = FMortFleetArea["FRetainFleetArea"];
      // FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
      // FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
      // 
      // // Calc Spawning Production and Spawning Biomass
      // // (first calculate by area and then summed over areas)
      // arma::cube FDeadFleetArea = FMortFleetArea["FDeadFleetArea"];
      // arma::mat FDeadAtAgeArea = arma::sum(FDeadFleetArea,1);
      // 
      // arma::vec SProductSBiomass = CalcSpawnProduction(NumberAtAgeArea.col(TSindex), // nAge
      //                                                  FecundityAtAge.col(TSindex), // nAge
      //                                                  MaturityAtAge.col(TSindex), // nAge
      //                                                  WeightAtAge.col(TSindex), // nAge
      //                                                  NaturalMortalityAtAge.col(TSindex), // nAge
      //                                                  FDeadAtAgeArea,
      //                                                  SpawnTimeFrac[st] // double
      // );
      // 
      // SProduction[TSindex] = SProductSBiomass[0];
      // SBiomass[TSindex] = SProductSBiomass[1];
      // SProductionList[st] = SProduction;
      // SBiomassList[st] = SBiomass;
     
    } // end of stock loop
    
    
    // apply SPFrom for spawning production from another stock 
    // TODO herm 
    if (nStock>1) {
      for (int st=0; st<nStock; st++) {
        int SPFrom = SPFromList[st];
        SProductionList[st] = SProductionList[SPFrom];
      }
    } 

    // Do Recruitment and Numbers at beginning of next time step
    for (int st=0; st<nStock; st++) {
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea

      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;

      // Calculate Recruitment
      NumericVector SProduction = SProductionList[st]; // nTS
      NumericVector R0 = R0List[st]; // nTS
      NumericVector SP0 = SP0List[st]; // nTS
      NumericVector RecDevs = RecDevsList[st]; // nTS
      Function SRRModel = SRRModelList[st]; // SRR R Function
      List SRRPars = SRRParsList[st];

      // NOTE: uses SP0 and R0 from first time step
      // Uses aggregate SProduction - ie summed over areas
      // TODO option to use time-varying alpha, beta
      double Recruits = CalcRecruitment_(SProduction[TSindex],
                                         R0[0],
                                         SP0[0],
                                         RecDevs[TSindex],
                                         SRRModel,
                                         SRRPars);


      // Distribute Recruits
      arma::cube UnfishedDist = UnfishedDistList[st]; // nage, nTS, nArea
      Rcpp::RObject AgesS4 = AgesList[st];
      bool plusgroup = AgesS4.slot("PlusGroup");
      arma::vec AgeClasses = AgesS4.slot("Classes");

      // Need to check if first age class is 0 or 1;
      int AgeRec = AgeClasses.min(); // first age class = age of recruitment (should be 0 or 1)
      int TSRec = TSindex + AgeRec; // TSindex + 1 for age-1 recruitment

      arma::vec recruitArea(nArea);

      for (int area=0; area<nArea; area++) {
        recruitArea(area) = Recruits * arma::as_scalar(UnfishedDist(arma::span(0, 0), arma::span(TSRec, TSRec), arma::span(area, area)));
      }

      if (TSRec == TSindex) {
        NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
      } else {
        if (timestep<(nTS-1))
          NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
      }
      
      if (timestep<(nTS-1)) {
        // Update Number beginning of next Time Step
        List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];

        arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
        arma::mat Semelparous = SemelparousList[st]; // nAge, nTS

        NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(NumberAtAgeArea.col(TSindex),
                                                         Semelparous.col(TSindex),
                                                         FDeadAtAgeAreaStock[TSindex],
                                                         NaturalMortalityAtAge.col(TSindex),
                                                         plusgroup,
                                                         nAge,
                                                         nArea
                                                         );

        // replace recruits if recruitment to age 1
        if (AgeRec > 0) {
          NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
        }

        // Move Population at beginning of next Time Step
        List MovementStock = MovementList[st];
        NumberAtAgeAreaList[st] = MoveStock_(NumberAtAgeArea, MovementStock[TSindex+1], nAge, nArea, TSindex+1);
      } else {
        NumberAtAgeAreaList[st] = NumberAtAgeArea;
      }
    } // end of stock loop
  } // end of time steps loop

  OMListSimOut["NumberAtAgeArea"] = NumberAtAgeAreaList;
  OMListSimOut["Biomass"] = BiomassList;
  OMListSimOut["VBiomassArea"] = VBiomassAreaList;
  OMListSimOut["EffortArea"] = EffortAreaList;
  OMListSimOut["SBiomass"] = SBiomassList;
  OMListSimOut["SProduction"] = SProductionList;
  OMListSimOut["FDeadAtAgeArea"] = FDeadAtAgeAreaList;
  OMListSimOut["FRetainAtAgeArea"] = FRetainAtAgeAreaList;
  
  
  return(OMListSimOut);
}


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



