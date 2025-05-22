#ifndef calculate_h
#define calculate_h

#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

inline double CalcBiomass(arma::mat NumberAtAgeArea, // nAge, nArea
                          arma::vec WeightAtAge){ // nAge
  
  double B = arma::accu(WeightAtAge %  arma::sum(NumberAtAgeArea,1));
  return(B);
}

inline arma::mat TransposeMatrix(arma::mat matrix, int ncol) {
  if (ncol == 1) {
    matrix = matrix.t();
  }
  return(matrix);
}

inline arma::mat CalcVBiomass(arma::mat NumberAtAgeArea, // nAge, nArea
                       arma::mat FleetWeightAtAgeFleet, // nAge, nFleet
                       arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                       arma::mat ClosureFleetArea) { // nFleet, nArea) 
  
  int nAge = NumberAtAgeArea.n_rows;
  int nArea = NumberAtAgeArea.n_cols;
  int nFleet = FleetWeightAtAgeFleet.n_cols;
  
  ClosureFleetArea = TransposeMatrix(ClosureFleetArea, nArea);
 
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


inline arma::mat CalcEffortDistribution(arma::mat VBiomassFleetArea, // nFleet, nArea
                                        arma::vec Effort,
                                        int nArea) {  // nFleet
  
  VBiomassFleetArea = TransposeMatrix(VBiomassFleetArea, nArea);
  int nFleet = VBiomassFleetArea.n_rows;
  
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

inline List CalcFMortality(arma::mat EffortFleetArea, // nFleet, nArea
                           arma::vec Catchability, // nFleet
                           arma::rowvec RelativeSize, // nArea
                           arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                           arma::mat RetentionAtAgeFleet, // nAge, nFleet
                           arma::mat DiscardMortalityAtAgeFleet, // nAge, nFleet
                           int nArea
) {
  

  EffortFleetArea = TransposeMatrix(EffortFleetArea, nArea);
  int nFleet = EffortFleetArea.n_rows;
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


inline arma::vec CalcSpawnProduction(arma::mat NumberAtAgeArea, // nAge, nArea
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


inline double CalcRecruitment_(double SProduction,
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


inline arma::mat CalcNumberNext_(arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
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


arma::cube CalcStockMovement_(arma::cube NumberAtAgeArea,
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

#endif // calculate_h