#ifndef BACK_CALCULATE_EFFORT_H
#define BACK_CALCULATE_EFFORT_H

#include <Rcpp.h>
#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

inline void BackCalculateEffort(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    const Array4D& FInteract,                   // sim, stock, year, fleet
    const ConstArrayView4D& q,                  // sim, stock, year, fleet          
    Array3D& Effort,                            // sim, year, fleet
    const int nStock,
    const int nFleet
) {
  
  for (int sim : Sims) {
    
    const int sim_q   = sim_index<4>(sim, q, "q");
    
    for (int fl = 0; fl < nFleet; ++fl) {
      double max_effort = 0.0;
      
    for (int st = 0; st < nStock; ++st) {
      const double q_val = q(sim_q, st, y, fl);
      if (q_val <= 0.0) continue;
      
      const double effort = FInteract(sim, st, y, fl) / q_val;
      max_effort = std::max(max_effort, effort);
    }
    Effort(sim, y, fl) = max_effort;
    }
  }
}
#endif
 