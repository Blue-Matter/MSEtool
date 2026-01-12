// #include <RcppArmadillo.h>
// using namespace Rcpp;
// 
// // [[Rcpp::export]]
// arma::cube CalcVBiomass_(
//     const Rcpp::List& NumberAtAgeAreaList,
//     const Rcpp::List& FleetList,
//     int TSindex,
//     int nStock,
//     int nFleet,
//     int nArea,
//     int debug = 0) {
//   if (debug) {
//     Rcout << "Start CalcVBiomass\n";
//   }
//   
//   arma::cube VBiomassStockFleetArea(nStock, nFleet, nArea, arma::fill::zeros);
//   
//   for (int st = 0; st < nStock; ++st) {
//     const arma::cube& NumberAtAgeArea = as<arma::cube>(NumberAtAgeAreaList[st]); // nAge x nTS x nArea 
//     const Rcpp::S4 Fleet = FleetList[st]; 
//     const arma::cube& FleetWeightAtAge = Fleet.slot("WeightFleet"); // nAge x nTS x nFleet
//     
//     const Rcpp::S4 Selectivity = Fleet.slot("Selectivity"); 
//     const arma::cube& SelectivityAtAge = Selectivity.slot("MeanAtAge"); // nAge x nTS x nFleet 
//     
//     const arma::cube& ClosureArea = Fleet.slot("Closure"); // nTS x nFleet x nArea
//     
//     const arma::mat Number = NumberAtAgeArea.col(TSindex); // nAge x nArea
//     const arma::mat FW     = FleetWeightAtAge.col(TSindex); // nAge x nFleet
//     const arma::mat Sel    = SelectivityAtAge.col(TSindex); // nAge x nFleet
//     
//     for (int fl = 0; fl < nFleet; ++fl) {
//       const arma::rowvec closure = ClosureArea.tube(TSindex, fl); // length nArea
//       
//       if (arma::all(closure == 0.0)) {
//         continue;
//       }
//       
//       const arma::vec age_weight = FW.col(fl) % Sel.col(fl); // nAge
//       const arma::vec biomass_by_area =  Number.t() * age_weight; // nArea
//       
//       for (int area = 0; area < nArea; ++area) {
//         const double c = closure(area);
//         if (c != 0.0) {
//           VBiomassStockFleetArea(st, fl, area) =
//             biomass_by_area(area) * c;
//         }
//       }
//     }
//   }
//   
//   if (debug) {
//     Rcout << "End CalcVBiomass\n";
//   } 
// 
//   return VBiomassStockFleetArea;
// }
