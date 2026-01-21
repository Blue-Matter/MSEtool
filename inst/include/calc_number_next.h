#ifndef CALC_NUMBER_NEXT_H
#define CALC_NUMBER_NEXT_H

#include <Rcpp.h>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Calculates numbers at beginning of next time step and distribute over aresa
inline void CalcNumberNext(
    const int y,
    const int nSim,
    std::vector<Array4D>& Number,
    const std::vector<Array5D>& FDeadArea,
    const std::vector<ConstArrayView3D> NaturalMortality,
    const std::vector<ConstArrayView3D> Semelparous,
    const ConstArrayView1D PlusGroup,
    const std::vector<ConstArrayView5D> Movement,
    const int nStock,
    const int nArea
) {
  
  
  
  
  for (int st = 0; st < nStock; ++st) {
    const auto& Num_st = Number[st];          // sim, age, year, area
    const auto& Fd = FDeadArea[st];           // sim, age, year, area
    const auto& M_st = NaturalMortality[st];  // sim, age, year
    const auto& Sem_st = Semelparous[st];     // sim, age, year
    const int plusgroup_st = PlusGroup(st);   // st
    const auto& Mov_st = Movement[st];        // sim, from, to, age, year
    
    const int simN  = Num_st.dim[0];
    const int simF  = Fd.dim[0];
    const int simM  = M_st.dim[0];
    const int simS  = Sem_st.dim[0];
    const int simMov  = Mov_st.dim[0];
    
    const int nAge = Num_st.dim[1];
    
    if (y < 0 || y >= Num_st.dim[2])
      Rcpp::stop("Year index y out of bounds in CalcNumberNext");
    
    
    for (int sim = 0; sim < nSim; ++sim) {
      
      const int iM  = sim_i(sim, simM);
      const int iSem  = sim_i(sim, simS);
      const int iFd  = sim_i(sim, simF);
      
    
      for (int age=0; age<(nAge-1); ++age) {
        double M = M_st(iM, age, y);
        double Sem = Sem_st(iSem, age, y);
        
      for (int area = 0; area < nArea; ++area) {
        double F = Fd(iFd, age, y, area);
        double Z = F + M;
        
        // age and apply mortality
        
        
        
        // movement    
        
      }
    }
  }
}

#endif