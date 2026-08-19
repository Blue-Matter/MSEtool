#ifndef CALC_TRANSITION_H
#define CALC_TRANSITION_H

#include <Rcpp.h>
#include <vector>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Age-dependent reclassification of individuals from one stock's Number
// into another's (e.g. sequential hermaphroditism). 
// Runs once per year on the complete year y+1 numbers-at-age (post CalcNumberNext/CalcRecruitment),
// moving Number[from] -> Number[to] at every age/area via the precomputed hazard rate. 
inline void CalcTransition(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
    const std::vector<ConstArrayView3D>& TransitionHazard, // [pair] sim, age, year
    const std::vector<int>& TransitionToStock,         // [pair] 0-indexed stock
    const std::vector<int>& TransitionFromStock,        // [pair] 0-indexed stock
    const int nTransitionPair,
    const int nStock,
    const int nArea) {

  for (int p = 0; p < nTransitionPair; ++p) {

    const int from = TransitionFromStock[p];
    const int to   = TransitionToStock[p];

    auto& Num_from       = Number[from];
    auto& Num_to         = Number[to];
    const auto& Hazard_p = TransitionHazard[p];

    const int nAge  = Num_from.dim[1];
    const int nYear = Num_from.dim[2];

    if (y + 1 >= nYear) continue;

    for (int sim : Sims) {

      const int sim_from   = sim_index<4>(sim, Num_from);
      const int sim_to     = sim_index<4>(sim, Num_to);
      const int sim_hazard = sim_index<3>(sim, Hazard_p);

      for (int age = 0; age < nAge; ++age) {
        const double hazard = Hazard_p(sim_hazard, age, y + 1);
        if (hazard <= 0.0) continue;

        for (int ar = 0; ar < nArea; ++ar) {
          const double Nmov = Num_from(sim_from, age, y + 1, ar) * hazard;
          Num_to(sim_to, age, y + 1, ar)     += Nmov;
          Num_from(sim_from, age, y + 1, ar) -= Nmov;
        }
      }
    } // end sim loop
  } // end pair loop
}
#endif
