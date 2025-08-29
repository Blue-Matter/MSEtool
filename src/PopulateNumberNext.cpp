#include <RcppArmadillo.h>
#include "calculate.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;




// [[Rcpp::export]]
S4 PopulateNumberNext_(S4 HistSimIn, 
                       NumericVector TimeStep) {
  
  S4 HistSim = clone(HistSimIn);
  S4 OM = HistSim.slot("OM");
  List StockList = OM.slot("Stock");
  List FleetList = OM.slot("Fleet");
  
  
  NumericVector TimeStepsAll = OM.slot("TimeSteps");
  // int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeStep, TimeStepsAll);
  
  List NumberAtAgeAreaList = HistSim.slot("Number"); // nStock
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = HistSim.slot("FDeadAtAgeArea");
  
  int TSindex = MatchTS[0] -1;
  
  for (int st=0; st<nStock; st++) {
    
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
    int nAge = NumberAtAgeArea.n_rows;
    int nTSNumber = NumberAtAgeArea.n_cols;
    int nArea = NumberAtAgeArea.n_slices;
    
    if (TSindex < nTSNumber-1) {
      S4 Stock = StockList[st];
      S4 NaturalMortality = Stock.slot("NaturalMortality");
      arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
      
      S4 Maturity = Stock.slot("Maturity");
      arma::mat Semelparous = Maturity.slot("Semelparous");
      
      S4 Ages = Stock.slot("Ages");
      bool plusgroup = Ages.slot("PlusGroup");
      
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      
      // Calculate Number at beginning of next time step
      NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(NumberAtAgeArea.col(TSindex),
                          Semelparous.col(TSindex),
                          FDeadAtAgeAreaStock[TSindex],
                          NaturalMortalityAtAge.col(TSindex),
                          plusgroup,
                          nAge,
                          nArea);
      
      
      // Move Population at beginning of next Time Step
      S4 Spatial = Stock.slot("Spatial");
      List MovementList = Spatial.slot("Movement");
      NumberAtAgeArea = CalcStockMovement_(NumberAtAgeArea,
                                           MovementList[TSindex+1],
                                                       nAge,
                                                       nArea,
                                                       TSindex+1);
      
    }
    NumberAtAgeAreaList[st] = NumberAtAgeArea;
  }
  HistSim.slot("Number") = NumberAtAgeAreaList;
  
  return(HistSim);
}