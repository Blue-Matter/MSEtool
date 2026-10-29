#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include <array>
#include <algorithm>
#include "array_types.h"
#include "array_nd.h"


// ArrayViewND (mutable)

template <size_t N>
struct ArrayViewND {
  double* x;
  std::array<int, N> dim;
  std::array<int, N> strides;
  Rcpp::NumericVector _anchor;
  
  ArrayViewND(Rcpp::NumericVector vec, const std::array<int, N>& dim_)
    : x(REAL(vec)), dim(dim_), _anchor(vec)
  {
    strides[0] = 1;
    for (size_t d = 1; d < N; ++d) strides[d] = strides[d-1] * dim[d-1];
  }
  
  inline double& operator()(const std::array<int, N>& ind) {
    int flat = 0;
    for (size_t d = 0; d < N; ++d) flat += ind[d] * strides[d];
    return x[flat];
  }
  
  template <typename... Args>
  inline double& operator()(Args... args) {
    std::array<int, N> ind{ static_cast<int>(args)... };
    return (*this)(ind);
  }
};

// ConstArrayViewND

template <size_t N>
struct ConstArrayViewND {
  const double* x;
  std::array<int, N> dim;
  std::array<int, N> strides;
  Rcpp::NumericVector _anchor;
  
  ConstArrayViewND(Rcpp::NumericVector vec, const std::array<int, N>& dim_)
    : x(REAL(vec)), dim(dim_), _anchor(vec)
  {
    strides[0] = 1;
    for (size_t d = 1; d < N; ++d) strides[d] = strides[d-1] * dim[d-1];
  }
  
  [[nodiscard]] inline double operator()(const std::array<int, N>& ind) const {
    int flat = 0;
    for (size_t d = 0; d < N; ++d) flat += ind[d] * strides[d];
    return x[flat];
  }
  
  template <typename... Args>
  [[nodiscard]] inline double operator()(Args... args) const {
    std::array<int, N> ind{ static_cast<int>(args)... };
    return (*this)(ind);
  }
};

using ArrayView1D = ArrayViewND<1>;
using ArrayView2D = ArrayViewND<2>;
using ArrayView3D = ArrayViewND<3>;
using ArrayView4D = ArrayViewND<4>;
using ArrayView5D = ArrayViewND<5>;

using ConstArrayView1D = ConstArrayViewND<1>;
using ConstArrayView2D = ConstArrayViewND<2>;
using ConstArrayView3D = ConstArrayViewND<3>;
using ConstArrayView4D = ConstArrayViewND<4>;
using ConstArrayView5D = ConstArrayViewND<5>;

// dims_from_sexp

template <size_t N>
inline std::array<int, N> dims_from_sexp(SEXP sexp) {
  std::array<int, N> dim;
  int* d = INTEGER(Rf_getAttrib(sexp, R_DimSymbol));
  for (size_t i = 0; i < N; ++i) dim[i] = d[i];
  return dim;
}

// as_ArrayND

template <size_t N>
inline ArrayND<N> as_ArrayND(SEXP sexp) {
  return ArrayND<N>(sexp, dims_from_sexp<N>(sexp));
}

// as_ConstArrayViewND

template <size_t N>
inline ConstArrayViewND<N> as_ConstArrayViewND(SEXP sexp) {
  return ConstArrayViewND<N>(Rcpp::NumericVector(sexp), dims_from_sexp<N>(sexp));
}

// as_ArrayViewND

template <size_t N>
inline ArrayViewND<N> as_ArrayViewND(SEXP sexp) {
  return ArrayViewND<N>(Rcpp::NumericVector(sexp), dims_from_sexp<N>(sexp));
}

// GetMisc_ConstArrayView

template <size_t N>
inline ConstArrayViewND<N> GetMisc_ConstArrayView(Rcpp::S4& obj, const char* nm) {
  return as_ConstArrayViewND<N>(static_cast<SEXP>(
      static_cast<const Rcpp::List&>(obj.slot("Misc"))[nm]));
}

// StockList view helpers

template <size_t N>
inline ConstArrayViewND<N> view_ConstStockListND(const Rcpp::List& L, int st) {
  return as_ConstArrayViewND<N>(static_cast<SEXP>(L[st]));
}

inline ConstArrayView3D view_ConstStockList3D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<3>(L, st);
}

inline ConstArrayView4D view_ConstStockList4D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<4>(L, st);
}

inline ConstArrayView5D view_ConstStockList5D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<5>(L, st);
}

inline ArrayView3D view_StockList3D(Rcpp::List& L, int st) {
  return as_ArrayViewND<3>(static_cast<SEXP>(L[st]));
}

inline ArrayView4D view_StockList4D(Rcpp::List& L, int st) {
  return as_ArrayViewND<4>(static_cast<SEXP>(L[st]));
}

inline ArrayView5D view_StockList5D(Rcpp::List& L, int st) {
  return as_ArrayViewND<5>(static_cast<SEXP>(L[st]));
}

#endif