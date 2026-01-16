#ifndef CALC_AREA_F_H
#define CALC_AREA_F_H

#include <Rcpp.h>
#include <array>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"


inline void CalcArea_F(
    const int y,                              // time-step index
    
    std::vector<Array5D>& FDeadArea,    // nStock vector: sim, age, year, fleet, area
    std::vector<Array5D>& FRetainArea,  // nStock vector: sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& Sel,
    const std::vector<ConstArrayView5D>& Ret,
    const std::vector<ConstArrayView5D>& DiscM,
  
    Array4D& EffortDist,                       // Prob Spatial Effort: sim, year, fleet, area
    const ConstArrayView4D& q,                         // Catchability array: sim, stock, year, fleet
    const Array3D& Effort,                    // Total Effort: sim, year, fleet
    const ConstArrayView2D& RelSize,                   // Relative Area Size; sim, area
    
    const int nStock,
    const int nFleet,
    const int nArea) {         
    

  // Calculate nSim - maximum number of simulations 
  const int nSim = infer_nSim(
    FDeadArea,
    FRetainArea,
    Sel,
    Ret,
    DiscM,
    EffortDist,
    Effort,
    q,
    RelSize
  );
  
  // single area  & single fleet
  if (nArea == 1 && nFleet == 1) {
    for (int st = 0; st < nStock; ++st) {
      auto& Fd  = FDeadArea[st];
      auto& Fr  = FRetainArea[st];
      const auto& S  = Sel[st];
      const auto& R  = Ret[st];
      const auto& DM = DiscM[st];
      
      const int nAge = Fd.dim[1];
      
      for (int sim = 0; sim < nSim; ++sim) {
        const double effort = Effort(sim, y, 0);
        const double q_eff  = q(sim, st, y, 0) * effort;
        
        for (int age = 0; age < nAge; ++age) {
          const double F_interact = q_eff * S(sim, age, y, 0, 0);
          const double F_retain   = F_interact * R(sim, age, y, 0, 0);
          const double F_disc     = (F_interact - F_retain) * DM(sim, age, y, 0, 0);
           
          Fd(sim, age, y, 0, 0) = F_retain + F_disc;
          Fr(sim, age, y, 0, 0) = F_retain;
        } 
      } 
    }
    return;
  }  
  
  // single area - multiple fleet
  if (nArea == 1) {
    for (int st = 0; st < nStock; ++st) {
      
      auto& Fd  = FDeadArea[st];
      auto& Fr  = FRetainArea[st];
      const auto& S  = Sel[st];
      const auto& R  = Ret[st];
      const auto& DM = DiscM[st];
    
      const int nAge = Fd.dim[1];
    
      for (int sim = 0; sim < nSim; ++sim) {
        for (int fl = 0; fl < nFleet; ++fl) {
         
          const double effort = Effort(sim, y, fl);
          const double q_eff  = q(sim, st, y, fl) * effort;
         
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * S(sim, age, y, fl, 0);
            const double F_retain   = F_interact * R(sim, age, y, fl, 0);
            const double F_disc     = (F_interact - F_retain) * DM(sim, age, y, fl, 0);
             
            Fd(sim, age, y, fl, 0) = F_retain + F_disc;
            Fr(sim, age, y, fl, 0) = F_retain;
          }
        }
      }
    } 
    return;
  } 
  
  // single fleet, multiple areas 
  if (nFleet == 1) {
    
    Array2D EffortDensity({nSim, nArea}, 0.0);
     
    for (int sim = 0; sim < nSim; ++sim) {
      const double E = Effort(sim, y, 0);
      for (int ar = 0; ar < nArea; ++ar) {
        const double rs = RelSize(sim, ar);
        EffortDensity(sim, ar) =
          (rs > 0.0) ? E * EffortDist(sim, y, 0, ar) / rs : 0.0;
      } 
    }
    
    for (int st = 0; st < nStock; ++st) {
       
      auto& Fd  = FDeadArea[st];
      auto& Fr  = FRetainArea[st];
      const auto& S  = Sel[st];
      const auto& R  = Ret[st];
      const auto& DM = DiscM[st];
       
      const int nAge = Fd.dim[1];
       
      for (int sim = 0; sim < nSim; ++sim) {
        const double q_fl = q(sim, st, y, 0);
         
        for (int ar = 0; ar < nArea; ++ar) {
          const double q_eff = q_fl * EffortDensity(sim, ar);
           
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * S(sim, age, y, 0, ar);
            const double F_retain   = F_interact * R(sim, age, y, 0, ar);
            const double F_disc     = (F_interact - F_retain) * DM(sim, age, y, 0, ar);
             
            Fd(sim, age, y, 0, ar) = F_retain + F_disc;
            Fr(sim, age, y, 0, ar) = F_retain;
          } 
        }
      }
    }
    return;
  } 
  

  // multiple areas, multiple fleets 
  
  // Compute spatial effort density
  Array3D EffortDensity({nSim, nFleet, nArea}, 0.0);
  
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      const double E = Effort(sim, y, fl);
      for (int ar = 0; ar < nArea; ++ar) {
        const double rs = RelSize(sim, ar);
        EffortDensity(sim, fl, ar) =
          (rs > 0.0) ? E * EffortDist(sim, y, fl, ar) / rs : 0.0;
      }
    } 
  }
  
  for (int st = 0; st < nStock; ++st) {
     
    auto& Fd  = FDeadArea[st];
    auto& Fr  = FRetainArea[st];
    const auto& S  = Sel[st];
    const auto& R  = Ret[st];
    const auto& DM = DiscM[st];
     
    const int nAge = Fd.dim[1];
     
    for (int sim = 0; sim < nSim; ++sim) {
      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_fl = q(sim, st, y, fl);
         
        for (int ar = 0; ar < nArea; ++ar) {
          const double q_eff = q_fl * EffortDensity(sim, fl, ar);
           
          for (int age = 0; age < nAge; ++age) {
            const double F_interact = q_eff * S(sim, age, y, fl, ar);
            const double F_retain   = F_interact * R(sim, age, y, fl, ar);
            const double F_disc     = (F_interact - F_retain) * DM(sim, age, y, fl, ar);
             
            Fd(sim, age, y, fl, ar) = F_retain + F_disc;
            Fr(sim, age, y, fl, ar) = F_retain;
          }
        }
      }
    }
  }
}


#endif // CALC_AREA_F_H