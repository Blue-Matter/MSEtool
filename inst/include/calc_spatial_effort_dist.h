#ifndef CALC_SPATIAL_DISTRIBUTION_H
#define CALC_SPATIAL_DISTRIBUTION_H

#include <Rcpp.h>
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// Mean fraction of biomass available over a time step under fishing mortality F
inline double DepletionDiscount(const double F) {
  if (F > 1e-6) return -std::expm1(-F) / F;
  return 1.0 - 0.5 * F + F * F / 6.0;
}

// -dlog h / dlog F
inline double DepletionElasticity(const double F) {
  if (F > 1e-6) return 1.0 - F * std::exp(-F) / -std::expm1(-F);
  return 0.5 * F;
}

// D[f,a] ~ w[f,a] * u[f,a]^lambda[f], u = sum_st b * h(F[st,a]) / w, F from all fleets in this time step
inline void CalcSpatialDistribution(
    const int y,
    const std::vector<int>& Sims,
    const int nSim,
    Array4D& Distribution,                                // sim, year, fleet, area
    const std::vector<Array4D>& Number,                   // [stock] sim, age, year, area
    const std::vector<ConstArrayView4D>& WeightFleetRetained,     // [stock] sim, age, year, fleet
    const std::vector<ConstArrayView4D>& WeightFleetSelected,     // [stock] sim, age, year, fleet
    const std::vector<ConstArrayView5D>& SelAge,          // [stock] sim, age, year, fleet, area
    const std::vector<ConstArrayView5D>& RetAge,          // [stock] sim, age, year, fleet, area
    const ConstArrayView4D& q,                            // sim, stock, year, fleet
    const ConstArrayView5D& Closure,                      // sim, stock, year, fleet, area
    const ConstArrayView3D& Spatial_Targeting,            // sim, year, fleet  (lambda >= 0)
    const ConstArrayView3D& ReleaseValue,                 // sim, year, fleet  (>= 0)
    const Array3D& Effort,                                // sim, year, fleet
    const ConstArrayView2D& RelSize,                      // sim, area  (sums to 1)
    const std::vector<bool>& UseDensity,                  // [fleet]
    const ConstArrayView4D& StockTargeting,               // sim, stock, fleet, year
    const bool StockTargetingFlag,
    const double maxF,
    const int nStock,
    const int nFleet,
    const int nArea) {

  if (nArea == 1) {
    for (int sim : Sims)
      for (int fl = 0; fl < nFleet; ++fl)
        Distribution(sim, y, fl, 0) = 1.0;
    return;
  }

  constexpr int    MaxIter = 1000;
  constexpr double Tol     = 1e-9;

  const int FA  = nFleet * nArea;
  const int SFA = nStock * FA;
  const int SA  = nStock * nArea;

  std::vector<double> b(SFA), bsel(SFA), coef(nStock * nFleet);
  std::vector<double> w(FA), D(FA), Dnew(FA), logT(FA), u(FA);
  std::vector<double> Fst(SA), hst(SA);
  std::vector<char>   isFree(FA), isOpen(FA);
  std::vector<double> freeMass(nFleet), lambda(nFleet);
  std::vector<char>   active(nFleet);

  auto ia  = [&](int fl, int ar) { return fl * nArea + ar; };
  auto isa = [&](int st, int fl, int ar) { return st * FA + fl * nArea + ar; };

  for (int sim : Sims) {
    const int sim_rs = sim_index<2>(sim, RelSize);
    const int sim_q  = sim_index<4>(sim, q);
    const int sim_cl = sim_index<5>(sim, Closure);
    const int sim_t  = sim_index<3>(sim, Spatial_Targeting);
    const int sim_rv = sim_index<3>(sim, ReleaseValue);
    const int sim_st = sim_index<4>(sim, StockTargeting);

    std::fill(b.begin(), b.end(), 0.0);
    std::fill(bsel.begin(), bsel.end(), 0.0);
    std::fill(isOpen.begin(), isOpen.end(), 0);

    // exploitable biomass valued as retained + ReleaseValue * released
    for (int st = 0; st < nStock; ++st) {
      const Array4D&          Num_st  = Number[st];
      const ConstArrayView4D& Wgt_st  = WeightFleetRetained[st];
      const ConstArrayView4D& WgtS_st = WeightFleetSelected[st];
      const ConstArrayView5D& Sel_st  = SelAge[st];
      const ConstArrayView5D& Ret_st  = RetAge[st];
      const int nAge     = Num_st.dim[1];
      const int sim_num  = sim_index<4>(sim, Num_st);
      const int sim_wgt  = sim_index<4>(sim, Wgt_st);
      const int sim_wgts = sim_index<4>(sim, WgtS_st);
      const int sim_sel  = sim_index<5>(sim, Sel_st);
      const int sim_ret  = sim_index<5>(sim, Ret_st);

      for (int fl = 0; fl < nFleet; ++fl) {
        const double q_val = q(sim_q, st, y, fl);
        if (q_val <= 0.0) continue;
        const double omega = ReleaseValue(sim_rv, y, fl);

        for (int ar = 0; ar < nArea; ++ar) {
          if (Closure(sim_cl, st, y, fl, ar) <= 0.0) continue;
          isOpen[ia(fl, ar)] = 1;

          double B_val = 0.0, B_sel = 0.0;
          for (int age = 0; age < nAge; ++age) {
            const double NS = Num_st(sim_num, age, y, ar) * Sel_st(sim_sel, age, y, fl, ar);
            if (NS <= 0.0) continue;
            const double r  = Ret_st(sim_ret, age, y, fl, ar);
            const double Ws = WgtS_st(sim_wgts, age, y, fl);
            B_val += NS * r * Wgt_st(sim_wgt, age, y, fl);
            if (omega > 0.0) B_val += omega * NS * (1.0 - r) * Ws;
            B_sel += NS * Ws;
          }
          b[isa(st, fl, ar)]    = q_val * B_val;
          bsel[isa(st, fl, ar)] = q_val * B_sel;
        }
      }
    }

    // a fleet that values nothing in any area is attracted by the biomass it selects
    for (int fl = 0; fl < nFleet; ++fl) {
      double total = 0.0;
      for (int st = 0; st < nStock; ++st)
        for (int ar = 0; ar < nArea; ++ar)
          total += b[isa(st, fl, ar)];
      if (total > 0.0) continue;
      for (int st = 0; st < nStock; ++st)
        for (int ar = 0; ar < nArea; ++ar)
          b[isa(st, fl, ar)] = bsel[isa(st, fl, ar)];
    }

    for (int fl = 0; fl < nFleet; ++fl) {
      lambda[fl] = std::max(0.0, Spatial_Targeting(sim_t, y, fl));
      double fixedMass = 0.0;
      bool anyFree = false;
      for (int ar = 0; ar < nArea; ++ar) {
        const int i = ia(fl, ar);
        const double A = RelSize(sim_rs, ar);
        w[i] = UseDensity[fl] ? std::max(A, 0.0) : 1.0;
        const double d = Distribution(sim, y, fl, ar);
        isFree[i] = std::isnan(d);
        if (isFree[i]) {
          anyFree = true;
          D[i] = 0.0;
        } else {
          D[i] = d;
          fixedMass += d;
        }
      }
      freeMass[fl] = std::max(0.0, 1.0 - fixedMass);
      active[fl]   = anyFree;

      const double E = Effort(sim, y, fl);
      for (int st = 0; st < nStock; ++st) {
        const double q_val = q(sim_q, st, y, fl);
        const double targ  = StockTargetingFlag ? StockTargeting(sim_st, st, fl, y) : 1.0;
        coef[st * nFleet + fl] = (q_val > 0.0 && E > 0.0) ? q_val * E * targ : 0.0;
      }
    }

    // log target weight of each free cell; -inf excludes it
    auto Target = [&](int fl) {
      for (int ar = 0; ar < nArea; ++ar) {
        const int i = ia(fl, ar);
        logT[i] = -INFINITY;
        if (!isFree[i] || !isOpen[i] || w[i] <= 0.0) continue;
        if (lambda[fl] <= 0.0) {
          logT[i] = std::log(w[i]);
        } else if (u[i] > 0.0) {
          logT[i] = std::log(w[i]) + lambda[fl] * std::log(u[i]);
        }
      }
    };

    // exp(logx) over the free cells of fleet fl, scaled to its free mass
    auto Normalise = [&](int fl, std::vector<double>& out, const std::vector<double>& logx) {
      double mx = -INFINITY;
      for (int ar = 0; ar < nArea; ++ar)
        if (isFree[ia(fl, ar)]) mx = std::max(mx, logx[ia(fl, ar)]);
      double sum = 0.0;
      for (int ar = 0; ar < nArea; ++ar) {
        const int i = ia(fl, ar);
        if (!isFree[i]) continue;
        out[i] = std::isfinite(logx[i]) ? std::exp(logx[i] - mx) : 0.0;
        sum += out[i];
      }
      for (int ar = 0; ar < nArea; ++ar) {
        const int i = ia(fl, ar);
        if (isFree[i]) out[i] = sum > 0.0 ? out[i] / sum * freeMass[fl] : 0.0;
      }
    };

    auto Utility = [&](bool deplete) {
      for (int st = 0; st < nStock; ++st)
        for (int ar = 0; ar < nArea; ++ar) {
          double F = 0.0;
          if (deplete)
            for (int fl = 0; fl < nFleet; ++fl) {
              const int i = ia(fl, ar);
              if (w[i] > 0.0) F += coef[st * nFleet + fl] * D[i] / w[i];
            }
          Fst[st * nArea + ar] = std::min(F, maxF);
          hst[st * nArea + ar] = DepletionDiscount(Fst[st * nArea + ar]);
        }
      for (int fl = 0; fl < nFleet; ++fl)
        for (int ar = 0; ar < nArea; ++ar) {
          const int i = ia(fl, ar);
          double U = 0.0;
          if (w[i] > 0.0)
            for (int st = 0; st < nStock; ++st)
              U += b[isa(st, fl, ar)] * hst[st * nArea + ar];
          u[i] = w[i] > 0.0 ? U / w[i] : 0.0;
        }
    };

    // start from the allocation without depletion
    Utility(false);
    double lambdaMax = 0.0;
    for (int fl = 0; fl < nFleet; ++fl) {
      if (!active[fl]) continue;
      Target(fl);
      Normalise(fl, D, logT);
      lambdaMax = std::max(lambdaMax, lambda[fl]);
    }

    // damped fixed-point iteration in log space; gamma keeps the update a contraction
    for (int it = 0; lambdaMax > 0.0 && it < MaxIter; ++it) {
      Utility(true);
      double eMax = 0.0;
      for (double F : Fst) eMax = std::max(eMax, DepletionElasticity(F));
      if (eMax <= 0.0) break;
      const double gamma = 1.0 / (1.0 + lambdaMax * eMax);

      double resid = 0.0;
      for (int fl = 0; fl < nFleet; ++fl) {
        if (!active[fl] || lambda[fl] <= 0.0) continue;
        Target(fl);
        Normalise(fl, Dnew, logT);
        for (int ar = 0; ar < nArea; ++ar)
          if (isFree[ia(fl, ar)])
            resid = std::max(resid, std::fabs(Dnew[ia(fl, ar)] - D[ia(fl, ar)]));
        for (int ar = 0; ar < nArea; ++ar) {
          const int i = ia(fl, ar);
          if (!isFree[i]) continue;
          logT[i] = (D[i] > 0.0 && std::isfinite(logT[i]))
            ? (1.0 - gamma) * std::log(D[i]) + gamma * logT[i]
            : -INFINITY;
        }
        Normalise(fl, Dnew, logT);
        for (int ar = 0; ar < nArea; ++ar)
          if (isFree[ia(fl, ar)]) D[ia(fl, ar)] = Dnew[ia(fl, ar)];
      }
      if (resid < Tol) break;
    }

    for (int fl = 0; fl < nFleet; ++fl)
      for (int ar = 0; ar < nArea; ++ar)
        if (isFree[ia(fl, ar)])
          Distribution(sim, y, fl, ar) = D[ia(fl, ar)];
  }
}

#endif // CALC_SPATIAL_DISTRIBUTION_H
