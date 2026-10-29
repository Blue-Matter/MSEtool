#ifndef BACK_CALC_EFFORT_H
#define BACK_CALC_EFFORT_H

#include <Rcpp.h>
#include <vector>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Overwrite Effort with the effort implied by the F actually realised.
//
// CalcArea_F sets F_interact(age) = q_eff * sel(age), with
//
//   q_eff(st,fl,ar) = q * E * Dist(fl,ar) * targ * scale(st,ar) [/ RelSize(ar)]
//
// `scale` is the maxF clamp and is the only term that breaks proportionality
// with E, so inverting the rest recovers the effort consistent with the F the
// population actually experienced. Without this a fleet chasing an uncatchable
// TAC reports an effort of `maxEffort` that no F ever corresponded to.
//
// Areas partition effort (Dist sums to 1 across areas, enforced by
// .CheckHistMisc()), so areas are summed. Stocks share the same scalar effort,
// so each yields an independent estimate and the largest is taken - a stock
// whose F was clamped understates the effort actually deployed, and any
// unclamped stock recovers E exactly.
//
// Dividing by `targ` makes each stock's estimate invariant to stock targeting,
// which is what allows them to be compared at all.
inline void BackCalcEffort(
    const int y,
    const std::vector<int>& Sims,
    Array3D& Effort,                                  // sim, year, fleet
    const std::vector<Array5D>& FInteractArea,        // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& SelAge,      // [stock] sim, age, year, fleet, area
    const ConstArrayView4D& q,                        // sim, stock, year, fleet
    const ConstArrayView2D& RelSize,                  // sim, area
    const std::vector<bool>& UseDensity,              // [fleet]
    const ConstArrayView4D& StockTargeting,           // sim, stock, fleet, year
    const bool StockTargetingFlag,
    const int nStock,
    const int nFleet,
    const int nArea,
    const std::vector<char>& Clamped) {               // from CalcArea_F

  for (int sim : Sims) {

    const int sim_ef = sim_index<3>(sim, Effort);
    const int sim_q  = sim_index<4>(sim, q);
    const int sim_rs = sim_index<2>(sim, RelSize);
    const int sim_st = sim_index<4>(sim, StockTargeting);

    for (int fl = 0; fl < nFleet; ++fl) {

      // Untouched by the maxF clamp: the requested effort already is the
      // realised effort, so recomputing it would only add rounding noise
      if (!Clamped[sim * nFleet + fl]) continue;

      double best = -1.0;

      for (int st = 0; st < nStock; ++st) {

        const double q_fl = q(sim_q, st, y, fl);
        if (q_fl <= 0.0) continue;

        const double targ =
          StockTargetingFlag ? StockTargeting(sim_st, st, fl, y) : 1.0;
        if (targ <= 0.0) continue;

        const auto& Fi = FInteractArea[st];
        const auto& S  = SelAge[st];

        const int nAge    = Fi.dim[1];
        const int sim_sel = sim_index<5>(sim, S);

        double sum_F  = 0.0;
        bool   any_ar = false;

        for (int ar = 0; ar < nArea; ++ar) {

          // Recover apical F as F_interact / sel at the most-selected age.
          // Taking the ratio rather than max(F_interact) avoids assuming
          // selectivity is standardised to 1 in every area.
          int    age_max = -1;
          double sel_max = 0.0;
          for (int age = 0; age < nAge; ++age) {
            const double sel = S(sim_sel, age, y, fl, ar);
            if (sel > sel_max) { sel_max = sel; age_max = age; }
          }
          if (age_max < 0) continue;   // fleet does not select this stock here

          const double q_eff = Fi(sim, age_max, y, fl, ar) / sel_max;
          const double w     = UseDensity[fl] ? RelSize(sim_rs, ar) : 1.0;

          sum_F += q_eff * w;
          any_ar = true;
        }

        if (!any_ar) continue;

        const double E_st = sum_F / (q_fl * targ);
        if (E_st > best) best = E_st;
      }

      // No stock gave a usable estimate: leave the requested effort alone
      if (best >= 0.0) Effort(sim_ef, y, fl) = best;
    }
  }
}

#endif // BACK_CALC_EFFORT_H
