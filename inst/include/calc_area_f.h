#ifndef CALC_AREA_F_H
#define CALC_AREA_F_H

#include <Rcpp.h>
#include <array>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void CalcArea_F(
    const int y,
    const int nSim,
    std::vector<Array5D>& FDeadArea,
    std::vector<Array5D>& FRetainArea,
    const std::vector<ConstArrayView5D>& SelAge,
    const std::vector<ConstArrayView5D>& RetAge,
    const std::vector<ConstArrayView5D>& DiscMort,
    const Array4D& Distribution,
    const ConstArrayView4D& q,
    const Array3D& Effort,
    const ConstArrayView2D& RelSize,
    const int nStock,
    const int nFleet,
    const int nArea) {

  if (Distribution.dim[1] <= y)
    Rcpp::stop("Distribution: y out of bounds");
  
  if (Effort.dim[1] <= y)
    Rcpp::stop("Effort: y out of bounds");
  
  if (q.dim[2] <= y)
    Rcpp::stop("q: y out of bounds");
  
  
  const int simD = Distribution.dim[0];
  const int simQ = q.dim[0];
  const int simE = Effort.dim[0];
  const int simR = RelSize.dim[0];
  
  // Calculate effort density
  Array3D EffortDensity({nSim, nFleet, nArea}, 0.0);
  for (int sim = 0; sim < nSim; ++sim) {
    
    const int iE = sim_i(sim, simE);
    const int iD = sim_i(sim, simD);
    const int iR = sim_i(sim, simR);
    
    for (int fl = 0; fl < nFleet; ++fl) {
      const double E = Effort(iE, y, fl);
      for (int ar = 0; ar < nArea; ++ar) {
        const double rs = RelSize(iR, ar);
        EffortDensity(sim, fl, ar) = (rs > 0.0) ? E * Distribution(iD, y, fl, ar) / rs  : 0.0;
      }
    }
  }
  
  // Calc F-at-age 
  for (int st = 0; st < nStock; ++st) {
    
    auto& Fd  = FDeadArea[st];
    auto& Fr  = FRetainArea[st];
  
    const auto& S  = SelAge[st];
    const auto& R  = RetAge[st];
    const auto& DM = DiscMort[st];
  
    const int nAge = Fd.dim[1];
   
    const int simS  = S.dim[0];
    const int simRt = R.dim[0];
    const int simDM = DM.dim[0];
    
    for (int sim = 0; sim < nSim; ++sim) {
      const int iQ  = sim_i(sim, simQ);
      const int iS  = sim_i(sim, simS);
      const int iRt = sim_i(sim, simRt);
      const int iDM = sim_i(sim, simDM);
      
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_fl = q(iQ, st, y, fl);
        
        for (int ar = 0; ar < nArea; ++ar) {
          const double q_eff = q_fl * EffortDensity(sim, fl, ar);
          if (q_eff <= 0.0) continue;
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * S(iS, age, y, fl, ar);
            const double F_retain = F_interact * R(iRt, age, y, fl, ar);
            const double F_disc = (F_interact - F_retain) * DM(iDM, age, y, fl, ar);
            Fd(sim, age, y, fl, ar) = F_retain + F_disc;
            Fr(sim, age, y, fl, ar) = F_retain;
          }
        }
      }
    }
  } 

}


#endif // CALC_AREA_F_H