#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]

double CalcRecruitment_(double SProduction,
                        double R0,
                        double SP0,
                        double RecDev,
                        Function SRRModel,
                        List SRRPars,
                        int TSindex) {
  
  // NOTE: uses SP0 and R0 from first time step
  // TODO option to use time-varying alpha, beta
  List Arglist = List::create(Named("S") = SProduction,
                              Named("S0") = SP0,
                              Named("R0") = R0);
  
  // Rcout << "Here" << std::endl;
  
  CharacterVector ParNames = SRRPars.names();
  CharacterVector ArglistNames(3+SRRPars.size());
  ArglistNames[0] = "S";
  ArglistNames[1] = "S0";
  ArglistNames[2] = "R0";
  
  for (int i=0; i<SRRPars.size(); i++) {
    NumericVector argVec = SRRPars[i];
    double arg = argVec[TSindex];
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