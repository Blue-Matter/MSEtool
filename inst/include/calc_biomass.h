#ifndef CALC_BIOMASS_H
#define CALC_BIOMASS_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcBiomass(
    const int y,    
    const std::vector<int>& Sims,
    const int nSim,
    Array3D& Biomass,                               // sim, stock, year
    const std::vector<Array4D>& Number,             // [stock] sim, age, year, area
    const std::vector<ConstArrayView3D> Weight,     // [stock] sim, age, year
    const int nStock,
    const int nArea) {
  
  // Checks
  if ((int)Number.size() < nStock ||
      (int)Weight.size() < nStock)
    Rcpp::stop("Stock-level input list shorter than nStock");
  
  check_dims<3>(Biomass, {nSim, nStock, Biomass.dim[2]}, "Biomass", y, 2);
  
  for (int st = 0; st < nStock; ++st) { 
    
    const Array4D& Num_st = Number[st];             // sim, age, year, area
    const ConstArrayView3D& Wght_st = Weight[st];   // sim, age, year
    const int nAge = Num_st.dim[1];
    
    check_dims<4>(Num_st, {nSim, nAge, Num_st.dim[2], nArea}, "Number", y, 2);
    check_dims<3>(Wght_st, {nSim, nAge, Wght_st.dim[2]}, "Weight", y, 2);
    
    for (int sim : Sims) {
      
      const int sim_num   = sim_index<4>(sim, Num_st, "Number");
      const int sim_wght  = sim_index<3>(sim, Wght_st, "Weight");
      
      Biomass(sim, st, y) = 0.0; 
      
      for (int age = 0; age < nAge; ++age) {
        double sum_area = 0.0;
        for (int area = 0; area < nArea; ++area) {
          sum_area += Num_st(sim_num, age, y, area);
        }
        Biomass(sim, st, y) += sum_area * Wght_st(sim_wght, age, y);
      }
    }
  }
}

#endif 