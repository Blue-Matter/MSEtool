#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
#include <vector>
#include "array_types.h"
#include "array_views.h"

inline std::vector<int>
  CalcTSIndex(const Rcpp::NumericVector& Years, const Rcpp::NumericVector& YearsAll) {
    Rcpp::IntegerVector matchTS = Rcpp::match(Years, YearsAll);
    const int nTS = Years.size();
    std::vector<int> ts_index(nTS);
    for (int ts = 0; ts < nTS; ++ts) {
      if (matchTS[ts] == NA_INTEGER)
        Rcpp::stop("Year not found in `YearsAll`");
      ts_index[ts] = matchTS[ts] - 1;
    }
    return ts_index;
  }

// Slot2ArrayND: wraps slot SEXP directly — no copy, anchored in ArrayND::x
inline Array2D Slot2Array2D(Rcpp::S4& obj, const char* slot) {
  return as_ArrayND<2>(static_cast<SEXP>(obj.slot(slot)));
}
inline Array3D Slot2Array3D(Rcpp::S4& obj, const char* slot) {
  return as_ArrayND<3>(static_cast<SEXP>(obj.slot(slot)));
}
inline Array4D Slot2Array4D(Rcpp::S4& obj, const char* slot) {
  return as_ArrayND<4>(static_cast<SEXP>(obj.slot(slot)));
}
inline Array5D Slot2Array5D(Rcpp::S4& obj, const char* slot) {
  return as_ArrayND<5>(static_cast<SEXP>(obj.slot(slot)));
}

template <size_t N, class ArrayType>
inline int sim_index(int sim, const ArrayType& arr) {
  return arr.dim[0] == 1 ? 0 : sim;
}

inline void check_years_argument(SEXP Years, const char* name) {
  if (!Rf_isNumeric(Years))
    Rcpp::stop(std::string("Argument `") + name + "` must be a numeric vector");
  if (Rf_length(Years) == 0)
    Rcpp::stop(std::string("Argument `") + name + "` cannot be empty");
}

inline void NormalizeSims(std::vector<int>& Sims, int nSim) {
  for (int& s : Sims) {
    if (s < 1 || s > nSim)
      Rcpp::stop("Sims contains out-of-range index: " + std::to_string(s) +
        " (nSim=" + std::to_string(nSim) + ")");
    --s;
  }
}

inline Rcpp::List DeepCloneList(const Rcpp::List& x) {
  int n = x.size();
  Rcpp::List out(n);
  for (int i = 0; i < n; i++) {
    SEXP elem = PROTECT(Rf_duplicate(x[i]));
    out[i] = elem;
    UNPROTECT(1);
  }
  if (x.hasAttribute("names"))
    out.attr("names") = x.attr("names");
  return out;
}

#endif // HELPERS_H