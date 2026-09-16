#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

#include "array_nd.h"
#include "array_types.h"

// [[Rcpp::export]]
Rcpp::List CalcCatchAtSizeFleet_(
    const Rcpp::NumericVector& key,          // Sim x Age x Class x Year x Area
    const Rcpp::IntegerVector& key_dim,
    const Rcpp::NumericVector& selectivity,  // Sim x Class x Year x Area (ignored if !sel_mode_length)
    const Rcpp::IntegerVector& sel_dim,
    const bool sel_mode_length,
    const Rcpp::NumericVector& landings_N,   // Sim x Age x Year x Area
    const Rcpp::NumericVector& discards_N,   // Sim x Age x Year x Area
    const Rcpp::IntegerVector& N_dim
) {
  std::array<int, 5> kd = {key_dim[0], key_dim[1], key_dim[2], key_dim[3], key_dim[4]};
  std::array<int, 4> nd = {N_dim[0], N_dim[1], N_dim[2], N_dim[3]};

  Array5D Key(key, kd);
  Array4D Nl(landings_N, nd);
  Array4D Nd(discards_N, nd);

  const int nSim   = N_dim[0];
  const int nAge   = kd[1];
  const int nClass = kd[2];
  const int nYear  = N_dim[2];
  const int nArea  = N_dim[3];

  std::array<int, 4> sd = {1, 1, 1, 1};
  if (sel_mode_length)
    sd = {sel_dim[0], sel_dim[1], sel_dim[2], sel_dim[3]};
  Array4D Sel(sel_mode_length ? selectivity : Rcpp::NumericVector(1, 0.0), sd);

  const bool sim_invariant_key = kd[0] == 1;
  const bool sim_invariant_sel = !sel_mode_length || sd[0] == 1;
  const bool sim_invariant     = sim_invariant_key && sim_invariant_sel;

  const double eps = std::numeric_limits<double>::epsilon();

  Rcpp::NumericVector LAS(static_cast<R_xlen_t>(nSim) * nClass * nYear * nArea);
  Rcpp::NumericVector DAS(static_cast<R_xlen_t>(nSim) * nClass * nYear * nArea);
  std::array<int, 4> out_dim = {nSim, nClass, nYear, nArea};
  Array4D Las(LAS, out_dim);
  Array4D Das(DAS, out_dim);

  std::vector<double> cond(static_cast<size_t>(nAge) * nClass);

  auto computeCond = [&](int sim, int area, int year) {
    if (sel_mode_length) {
      for (int a = 0; a < nAge; ++a) {
        double denom = 0.0;
        for (int c = 0; c < nClass; ++c) {
          const double w = Key(sim, a, c, year, area) * Sel(sim, c, year, area);
          cond[a + nAge * c] = w;
          denom += w;
        }
        if (denom == 0.0) denom = eps;
        for (int c = 0; c < nClass; ++c) {
          const double v = cond[a + nAge * c] / denom;
          cond[a + nAge * c] = std::isfinite(v) ? v : 0.0;
        }
      }
    } else {
      for (int a = 0; a < nAge; ++a)
        for (int c = 0; c < nClass; ++c)
          cond[a + nAge * c] = Key(sim, a, c, year, area);
    }
  };

  for (int area = 0; area < nArea; ++area) {
    for (int year = 0; year < nYear; ++year) {

      if (sim_invariant) computeCond(0, area, year);

      for (int sim = 0; sim < nSim; ++sim) {
        if (!sim_invariant) computeCond(sim, area, year);

        for (int c = 0; c < nClass; ++c) {
          double las = 0.0, das = 0.0;
          for (int a = 0; a < nAge; ++a) {
            const double cv = cond[a + nAge * c];
            las += cv * Nl(sim, a, year, area);
            das += cv * Nd(sim, a, year, area);
          }
          Las(sim, c, year, area) = las;
          Das(sim, c, year, area) = das;
        }
      }
    }
  }

  return Rcpp::List::create(Rcpp::Named("LAS") = LAS, Rcpp::Named("DAS") = DAS);
}
