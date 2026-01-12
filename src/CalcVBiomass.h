// #ifndef CALC_V_BIOMASS_H
// #define CALC_V_BIOMASS_H
// 
// #include <RcppArmadillo.h>
// 
// // This function computes vulnerable biomass by
// // stock × fleet × area for a given timestep.
// arma::cube CalcVBiomass_(
//     const Rcpp::List& NumberAtAgeAreaList, // length nStock, each nAge x nTS x nArea cube
//     const Rcpp::List& FleetList,            // length nStock, each an S4 Fleet object
//     int TSindex,                            // time step index
//     int nStock,
//     int nFleet,
//     int nArea,
//     int debug = 0
// );
// 
// #endif // CALC_V_BIOMASS_H