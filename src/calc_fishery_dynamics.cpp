#include <Rcpp.h>
#include <cmath>
#include <vector>

#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "hist_view.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"
#include "calc_spawn_production.h"
#include "calc_recruitment.h"
#include "calc_number_next.h"
#include "calc_biomass.h"
#include "calc_catch.h"
#include "calc_overall_f.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              SEXP Years, // Years to loop over
                              SEXP AllYears,
                              std::vector<int> Sims,
                              const int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int DoCalcCatch=1,        // calculate catch?
                              const int DoCalcaggF=1,         // calculate overall F?   
                              const int debug=0
) {
  
  
  if (debug) 
    Rcpp::Rcout << "Starting CalcFisheryDynamics_\n";
  
  // Checks 
  check_years_argument(Years, "Years");
  NumericVector years(Years);
  
  check_years_argument(AllYears, "AllYears");
  NumericVector all_years(AllYears);
  
  if (nSim < 1)
    Rcpp::stop("nSim < 1");
  
  if (nStock < 1)
    Rcpp::stop("nStock < 1");
  
  if (nFleet < 1)
    Rcpp::stop("nFleet < 1");
  
  if (nArea < 1)
    Rcpp::stop("nArea < 1");
  
  // zero index Sims
  NormalizeSims(Sims, nSim);
  
  // Clone hist object
  Rcpp::S4 Hist = Rcpp::clone(HistIn); 
  
  // create HistView object 
  HistView hv(Hist, nSim, nStock, nFleet, nArea);
  
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(years, all_years); // time-step index
  for (int i : ts_index) {
    if (i < 0 || i >= all_years.size())
      Rcpp::stop("Invalid time index " + std::to_string(i) + "from CalcTSIndex()");
  }
  int nTS = ts_index.size();
  
  if (debug) 
    Rcpp::Rcout << "Starting time step loop \n";

  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
  
    int y = ts_index[ts]; // index for this time step
    
    if (debug) {
      Rcpp::Rcout << "******** Begin Time Step ***************\n";
      Rcpp::Rcout << "ts = " << ts << "\n";
      Rcpp::Rcout << "y = " << y+1 << "\n";
    }
      
    
    // ---------------------------------------------------------
    // MICE Calculations - TODO 
    // 
    //  Note: currently all biological arrays are non-mutable
    //        and no life-history parameters are passed into the function.
    //        
    //        This will have to be revised once MICE calcs are added
    //
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Spatial Distribution of Fishing Effort
    // src: inst/include/calc_spatial_effort_dist.h
    // ---------------------------------------------------------
    
    if (debug) 
      Rcpp::Rcout << "Begin CalcSpatialDistribution \n";
    
    CalcSpatialDistribution(y,
                            Sims,
                            nSim,
                            hv.Distribution,
                            hv.Number,
                            hv.WeightFleet,
                            hv.SelAge,
                            hv.RetAge,
                            hv.q,
                            hv.Closure,
                            hv.Targeting,
                            hv.Effort,
                            hv.RelSize,
                            nStock,
                            nFleet,
                            nArea);
    
    if (debug) 
      Rcpp::Rcout << "End CalcSpatialDistribution \n";
    
    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // ---------------------------------------------------------
    
    if (debug)
      Rcpp::Rcout << "Begin CalcArea_F \n";

    CalcArea_F(y,
               Sims,
               nSim,
               hv.FDeadArea,
               hv.FRetainArea,
               hv.SelAge,
               hv.RetAge,
               hv.DiscMort,
               hv.Distribution,
               hv.q,
               hv.Effort,
               hv.RelSize,
               hv.maxF,
               nStock,
               nFleet,
               nArea);

    if (debug)
      Rcpp::Rcout << "End CalcArea_F \n";

    // ---------------------------------------------------------
    // Calculate Global Spawning Biomass and Spawning Production
    // src: inst/include/calc_spawn_production.h
    // ---------------------------------------------------------
    
    if (debug)
      Rcpp::Rcout << "Begin CalcSpawnProduction \n";

    CalcSpawnProduction(y,
                        Sims,
                        nSim,
                        hv.SBiomass,
                        hv.SProduction,
                        hv.Number,
                        hv.Fecundity,
                        hv.Maturity,
                        hv.Weight,
                        hv.NaturalMortality,
                        hv.SpawnTimeFrac,
                        hv.SPFrom,
                        hv.FDeadArea,
                        nStock,
                        nFleet,
                        nArea);

    if (debug)
      Rcpp::Rcout << "End CalcSpawnProduction \n";
    
    // ---------------------------------------------------------
    // Calculate Recruitment & Distribute over areas
    // src: inst/include/calc_recruitment.h
    // ---------------------------------------------------------
    
    if (debug)
      Rcpp::Rcout << "Begin CalcRecruitment \n";

    CalcRecruitment(y,
                    Sims,
                    nSim,
                    hv.Number,
                    hv.SProduction,
                    hv.SRR_Pars,
                    hv.SRR_Model,
                    hv.RecLag,
                    hv.RecDevs,
                    hv.SP0,
                    hv.R0,
                    hv.RecDist,
                    nStock,
                    nArea);

    if (debug)
      Rcpp::Rcout << "End CalcRecruitment \n";

    // ---------------------------------------------------------
    // Calculate Number at beginning of next time step
    // src: inst/include/calc_number_next.h
    // ---------------------------------------------------------
    
    if (debug)
      Rcpp::Rcout << "Begin CalcNumberNext \n";

    CalcNumberNext(y,
                   Sims,
                   nSim,
                   hv.Number,
                   hv.FDeadArea,
                   hv.NaturalMortality,
                   hv.Semelparous,
                   hv.PlusGroup,
                   hv.Movement,
                   nStock,
                   nFleet,
                   nArea);


    if (debug)
      Rcpp::Rcout << "End CalcNumberNext \n";

    // ---------------------------------------------------------
    // Calculate Biomass (this time step)
    // src: inst/include/calc_biomass.h
    // ---------------------------------------------------------
    
    
    if (debug)
      Rcpp::Rcout << "Begin CalcBiomass \n";

    CalcBiomass(y,
                Sims,
                nSim,
                hv.Biomass,
                hv.Number,
                hv.Weight,
                nStock,
                nArea);

    if (debug)
      Rcpp::Rcout << "End CalcBiomass \n";
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/calc_catch.h
    // ---------------------------------------------------------
  
    if (DoCalcCatch) {
      
      // TODO   - calc landings- and discards-at-size
      //        - need to calculate ASK internally
      //        - and first check if sel_len/wght exists
      
      
      if (debug)
        Rcpp::Rcout << "Begin CalcCatch \n";

      CalcCatch(y,
                Sims,
                nSim,
                hv.LandingsAtAge,
                hv.DiscardsAtAge,
                hv.Landings,
                hv.Discards,
                hv.FDeadArea,
                hv.FRetainArea,
                hv.NaturalMortality,
                hv.Number,
                hv.WeightFleet,
                nStock,
                nFleet,
                nArea);

      if (debug)
        Rcpp::Rcout << "End CalcCatch \n";
      
    }
    
    // ---------------------------------------------------------
    // Calculate overall F (if applicable)
    // src: inst/include/cacl_overall_f.h
    // ---------------------------------------------------------
    
    if (DoCalcaggF) {
      
      if (debug)
        Rcpp::Rcout << "Begin CalcOverallF \n";

      CalcOverallF(y,
                   Sims,
                   nSim,
                   hv.FDead,
                   hv.FRetain,
                   hv.LandingsAtAge,
                   hv.DiscardsAtAge,
                   hv.Number,
                   nStock,
                   nFleet,
                   nArea
                   );

      if (debug)
        Rcpp::Rcout << "End CalcOverallF \n";
      
    }
    
    
    if (debug) 
      Rcpp::Rcout << "******End Time Step ****\n\n";
  
    
  }
  return(Hist);
}


// Optimize a single-fleet log-effort to minimize the objective function
double OptimizeSingleFleet(std::function<double(double)> obj,
                           double logLow, double logHigh,
                           int maxIter = 50, double tol = 1e-6) {
  
  const double gr = (std::sqrt(5.0) - 1.0) / 2.0;
  double a = logLow;
  double b = logHigh;
  double c = b - gr * (b - a);
  double d = a + gr * (b - a);
  double fc = obj(c);
  double fd = obj(d);
  
  for (int iter = 0; iter < maxIter; ++iter) {
    if (std::abs(b - a) < tol) break;
    if (fc < fd) {
      b = d;
      d = c;
      fd = fc;
      c = b - gr * (b - a);
      fc = obj(c);
    } else { 
      a = c;
      c = d;
      fc = fd;
      d = a + gr * (b - a);
      fd = obj(d);
    }
  } 
  
  return (fc < fd) ? c : d;
} 
// 
// // [[Rcpp::export]]
// NumericVector OptimizeEffort(S4 Hist,
//                              std::vector<int> Sims,
//                              int TSIndex,
//                              NumericVector TAC_by_Fleet,
//                              SEXP Years,
//                              SEXP AllYears,
//                              std::vector<int> stocks,
//                              int nSim,
//                              int nStock,
//                              int nArea,
//                              double minEffort = 1e-2,
//                              double tol = 1e-2,
//                              int maxIter = 20) {
// 
//   int nFleet = TAC_by_Fleet.size();
//   NumericVector Effort_final(nFleet);
//   
//   int sim = Sims[0];
//   
//   // 0-index 
//   std::vector<int> stocks0 = stocks;
//   for (auto& st : stocks0) {
//     st -= 1;
//   }
//   
//   // construct HistView
//   HistView hv(Hist, nSim, nStock, nFleet, nArea);
//   
//   // initialize effort
//   for (int f = 0; f < nFleet; ++f) {
//     Effort_final[f] = hv.Effort(sim, TSIndex, f);
//     if (Effort_final[f] < minEffort) Effort_final[f] = minEffort;
//   }
// 
// 
//   // fleets with TAC>0
//   std::vector<int> pos_idx;
//   for (int f = 0; f < nFleet; ++f) if (TAC_by_Fleet[f] > 0) pos_idx.push_back(f);
//   // fleets with TAC==0
//   for (int f = 0; f < nFleet; ++f) if (TAC_by_Fleet[f] == 0) Effort_final[f] = 0;
// 
//   if (pos_idx.empty()) return Effort_final;
// 
//  
//   // single fleet: Brent-style log-space optimization
//   if (pos_idx.size() == 1) {
//     int f = pos_idx[0];
//     double logLow = std::log(minEffort / Effort_final[f]);
//     double logHigh = std::log(Effort_final[f] * 10 / Effort_final[f]);
//   
//     auto obj = [&](double logEff) -> double {
//       NumericVector EffCopy = clone(Effort_final);
//       EffCopy[f] *= std::exp(logEff);
//       for (int i = 0; i < nFleet; ++i) hv.Effort(sim, TSIndex, i) = EffCopy[i];
//       S4 Temp = CalcFisheryDynamics_(Hist,
//                                      Years,
//                                      AllYears,
//                                      Sims,
//                                      nSim,
//                                      nStock,
//                                      nFleet,
//                                      nArea,
//                                      1,
//                                      0,
//                                      0);
// 
//       double Rem = 0.0;
//       Array4D Landings = Temp.slot("Landings");
//       Array4D Discards = Temp.slot("Discards");
//       for (int st : stocks0) {
//         Rem += Landings(sim, st, TSIndex, f) + Discards(sim, st, TSIndex, f);
//       }
//       return std::pow(std::log(TAC_by_Fleet[f]) - std::log(Rem), 2);
//     };
//   
//     double bestLog = OptimizeSingleFleet(obj, logLow, logHigh);
//     Effort_final[f] *= std::exp(bestLog);
//     return Effort_final;
//   }
// 
//   
//   // multiple fleets: vectorized Newton-Raphson
//   NumericVector Effort = clone(Effort_final);
//   NumericVector deltaF(nFleet);
// 
//   for (int iter = 0; iter < maxIter; ++iter) {
//     for (int f = 0; f < nFleet; ++f) hv.Effort(sim, TSIndex, f) = Effort[f];
//     S4 Temp_base = CalcFisheryDynamics_(Hist,
//                                    Years,
//                                    AllYears,
//                                    Sims,
//                                    nSim,
//                                    nStock,
//                                    nFleet,
//                                    nArea,
//                                    1,
//                                    0,
//                                    0);
// 
//     Array4D Land_base = Temp_base.slot("Landings");
//     Array4D Disc_base = Temp_base.slot("Discards");
//      
//     NumericVector Rem_base(nFleet);
//     bool done = true;
//     for (int f : pos_idx) {
//       double Rem_f = 0.0;
//       for (int st : stocks0)  {
//         Rem_f += Land_base(sim, st, TSIndex, f) + Disc_base(sim, st, TSIndex, f); 
//       }
//       Rem_base[f] = Rem_f;
//       if (std::abs(TAC_by_Fleet[f] - Rem_base[f]) > tol) done = false;
//     }
//     if (done) break;
// 
//     
//     for (int f : pos_idx) deltaF[f] = std::max(Effort[f] * 1e-4, 1e-8);
//     NumericVector Eff_pert = clone(Effort);
// 
//     for (int f = 0; f < nFleet; ++f) Eff_pert[f] += deltaF[f];
//    
//    
// 
//    
//     for (int f = 0; f < nFleet; ++f) hv.Effort(sim, TSIndex, f) = Eff_pert[f];
//     S4 Temp_pert = CalcFisheryDynamics_(Hist,
//                                         Years,
//                                         AllYears,
//                                         Sims,
//                                         nSim,
//                                         nStock,
//                                         nFleet,
//                                         nArea,
//                                         1,
//                                         0,
//                                         0);
//     
//  
//     
//     Array4D Land_pert = Temp_pert.slot("Landings");
//     Array4D Disc_pert = Temp_pert.slot("Discards");
//    
//    for (int f : pos_idx) {
//      double Rem_pert_f = 0.0;
//      for (int st : stocks0) Rem_pert_f += Land_pert(sim, st, TSIndex, f) + Disc_pert(sim, st, TSIndex, f);
//      double dC = (Rem_pert_f - Rem_base[f]) / deltaF[f];
//      if (dC > 0) Effort[f] = std::max(Effort[f] + (TAC_by_Fleet[f] - Rem_base[f]) / dC, minEffort);
//      else {
//        double logAdj = std::log(TAC_by_Fleet[f] / std::max(Rem_base[f], 1e-8));
//        Effort[f] *= std::exp(logAdj);
//      }
//    }
//   }
//    
//   for (int f = 0; f < nFleet; ++f) Effort_final[f] = Effort[f];
//   return Effort_final;
// } 
