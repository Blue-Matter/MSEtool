#ifndef CALC_BIOMASS_H
#define CALC_BIOMASS_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcBiomass(
    const int y,    
    const int nSim,
    Array3D& Biomass,  // sim, stock, year
    const std::vector<Array4D>& Number,
    const std::vector<ConstArrayView3D> Weight,
    const int nStock,
    const int nArea) {
  
  for (int st = 0; st < nStock; ++st) { 
    
    const Array4D& Num_st = Number[st];             // sim, age, year, area
    const ConstArrayView3D& Wght_st = Weight[st];   // sim, age, year
    
    const int nAge = Num_st.dim[1];
    
    for (int sim = 0; sim < nSim; ++sim) {
      Biomass(sim, st, y) = 0.0; 
      
      for (int age = 0; age < nAge; ++age) {
        double sum_area = 0.0;
        for (int area = 0; area < nArea; ++area) {
          sum_area += Num_st(sim, age, y, area);
        }
        Biomass(sim, st, y) += sum_area * Wght_st(sim, age, y);
      }
    }
  }
}

#endif 