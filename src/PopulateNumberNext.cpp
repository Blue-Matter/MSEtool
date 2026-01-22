// #include <RcppArmadillo.h>
// #include "CalcNumberNext.h"
// #include "CalcStockMovement.h"
// 
// //[[Rcpp::depends(RcppArmadillo)]]
// using namespace Rcpp;
// 
// 
// 
// 
// // [[Rcpp::export]]
// S4 PopulateNumberNext_(S4 HistSimIn, 
//                        NumericVector Year,
//                        int debug=0) {
//   
//   S4 HistSim = clone(HistSimIn);
//   S4 OM = HistSim.slot("OM");
//   List StockList = OM.slot("Stock");
//   List FleetList = OM.slot("Fleet");
//   
//   
//   NumericVector YearsAll = OM.slot("Years");
//   // int nTS = Years.size();
//   IntegerVector MatchTS = match(Year, YearsAll);
//   
//   List NumberAtAgeAreaList = HistSim.slot("Number"); // nStock
//   int nStock = NumberAtAgeAreaList.size();
//   
//   List FDeadAtAgeAreaList = HistSim.slot("FDeadArea");
//   
//   int TSindex = MatchTS[0] -1;
//   
//   if (debug)
//     Rcout << "TSindex = " << TSindex << std::endl;
//   
//   for (int st=0; st<nStock; st++) {
//     
//     arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
//     int nAge = NumberAtAgeArea.n_rows;
//     int nTSNumber = NumberAtAgeArea.n_cols;
//     int nArea = NumberAtAgeArea.n_slices;
//     
//     if (debug)
//       Rcout << "nTSNumber = " << nTSNumber << std::endl;
//     
//     if (TSindex < nTSNumber-1) {
//       S4 Stock = StockList[st];
//       S4 NaturalMortality = Stock.slot("NaturalMortality");
//       arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
// 
//       S4 Maturity = Stock.slot("Maturity");
//       arma::mat Semelparous = Maturity.slot("Semelparous");
// 
//       S4 Ages = Stock.slot("Ages");
//       bool plusgroup = Ages.slot("PlusGroup");
// 
//       List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
// 
//       // Calculate Number at beginning of next time step
//       NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(
//         NumberAtAgeArea.col(TSindex),
//         NumberAtAgeArea.col(TSindex+1),
//         Semelparous.col(TSindex),
//         FDeadAtAgeAreaStock[TSindex],
//         NaturalMortalityAtAge.col(TSindex),
//         plusgroup,
//         nAge,
//         nArea);
// 
// 
//       // Move Population at beginning of next Time Step
//       S4 Spatial = Stock.slot("Spatial");
//       List MovementList = Spatial.slot("Movement");
//       NumberAtAgeArea = CalcStockMovement_(NumberAtAgeArea,
//                                            MovementList[TSindex+1],
//                                                        nAge,
//                                                        nArea,
//                                                        TSindex+1);
// 
//     }
//     NumberAtAgeAreaList[st] = NumberAtAgeArea;
//   }
//   HistSim.slot("Number") = NumberAtAgeAreaList;
//   
//   return(HistSim);
// }