#include <Rcpp.h>
#include "array3d.h"
#include "array4d.h"
#include "array5d.h"
#include "array_views.h"

// #include "../inst/include/array4d.h"
using namespace Rcpp;


// void DistributeEffort(Array5D& Dist) {
// 
// 
// 
//   // loop over Sim, Stock, Year, Fleet, and Area
// 
// }

// Vulnerable Biomass
Rcpp::NumericMatrix CalcVBiomass(NumericVector Num, // Sim, Age, Year, Area
                                 NumericVector Sel, // Sim, Age, Time, Area
                                 NumericVector Ret, // Sim, Age, Time, Area
                                 NumericVector Weight, // Sim, Age, Time 
                                 int TSindex, // time step index 
                                 int nSim,
                                 int nArea) { 
  
  NumericMatrix VB(nSim, nArea);
  
  // Num x Sel x Ret X Weight for a given time step and area
  
  return(VB);
}
  
  



// Expected CPUE / VB per Unit Effort - by fleet

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(S4 Hist,
                              Rcpp::NumericVector Years,
                              int CalcCatch = 1,
                              int debug = 0) {

  // Clone to avoid modifying Hist outside function scope
  Rcpp::S4 HistOut = clone(Hist);
  
  auto Effort = make_Array4D(HistOut, "Effort");
  auto Dist = make_Array5D(HistOut, "Distribution");
  
  
  return(HistOut);
}






