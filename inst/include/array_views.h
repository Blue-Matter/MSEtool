#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include <array>
#include <algorithm>
#include "array_types.h"
#include "array_nd.h"

using Array2D = ArrayND<2>;
using Array3D = ArrayND<3>;
using Array4D = ArrayND<4>;
using Array5D = ArrayND<5>;

// ArrayViewND (mutable)

template <size_t N>
struct ArrayViewND {
  double* x;
  std::array<int, N> dim;
  std::string name;
  Rcpp::NumericVector _anchor;  
  
  ArrayViewND(Rcpp::NumericVector vec, const std::array<int, N>& dim_,
              const std::string& nm = "")
    : x(REAL(vec)), dim(dim_), name(nm), _anchor(vec) {}

  inline double& operator()(const std::array<int, N>& ind) {
    int flat = 0, stride = 1;
    for (size_t d = 0; d < N; ++d) { flat += ind[d] * stride; stride *= dim[d]; }
    return x[flat];
  }

  template <typename... Args>
  inline double& operator()(Args... args) {
    static_assert(sizeof...(Args) == N, "Invalid number of indices");
    std::array<int, N> ind{ static_cast<int>(args)... };
    return (*this)(ind);
  }
}; 

// ConstArrayViewND 

template <size_t N>
struct ConstArrayViewND {
  const double* x;
  std::array<int, N> dim;
  std::string name;
  Rcpp::NumericVector _anchor;  

  ConstArrayViewND(Rcpp::NumericVector vec, const std::array<int, N>& dim_,
                   const std::string& nm = "")
    : x(REAL(vec)), dim(dim_), name(nm), _anchor(vec) {}

  inline double operator()(const std::array<int, N>& ind) const {
    int flat = 0, stride = 1;
    for (size_t d = 0; d < N; ++d) { flat += ind[d] * stride; stride *= dim[d]; }
    return x[flat];
  }
   
  template <typename... Args>
  inline double operator()(Args... args) const {
    static_assert(sizeof...(Args) == N, "Invalid number of indices");
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
inline std::array<int, N> dims_from_sexp(SEXP sexp, const std::string& name) {
  SEXP dim_sexp = Rf_getAttrib(sexp, R_DimSymbol);
  if (Rf_isNull(dim_sexp) || Rf_length(dim_sexp) != static_cast<int>(N))
    Rcpp::stop("Expected " + std::to_string(N) + "D array for: " + name);
  std::array<int, N> dim;
  int* d = INTEGER(dim_sexp);
  for (size_t i = 0; i < N; ++i) dim[i] = d[i];
  return dim;
} 

//  as_ArrayND
template <size_t N>
inline ArrayND<N> as_ArrayND(SEXP sexp, const std::string& name = "") {
  auto dim = dims_from_sexp<N>(sexp, name);
  return ArrayND<N>(sexp, dim, name);
} 

// as_ConstArrayViewND
// Single overload taking SEXP only.

template <size_t N>
inline ConstArrayViewND<N> as_ConstArrayViewND(SEXP sexp, const std::string& name = "") {
  auto dim = dims_from_sexp<N>(sexp, name);
  return ConstArrayViewND<N>(Rcpp::NumericVector(sexp), dim, name);
} 

// as_ArrayViewND 

template <size_t N>
inline ArrayViewND<N> as_ArrayViewND(SEXP sexp, const std::string& name = "") {
  auto dim = dims_from_sexp<N>(sexp, name);
  return ArrayViewND<N>(Rcpp::NumericVector(sexp), dim, name);
} 

// GetMisc_ConstArrayView 

template <size_t N>
inline ConstArrayViewND<N> GetMisc_ConstArrayView(Rcpp::S4& obj, const char* nm) {
  const Rcpp::List MiscL = obj.slot("Misc");
  if (!MiscL.containsElementNamed(nm))
    Rcpp::stop("Hist@Misc$" + std::string(nm) + " not found");
  return as_ConstArrayViewND<N>(static_cast<SEXP>(MiscL[nm]), nm);
}
 
//  StockList view helpers

template <size_t N>
inline ConstArrayViewND<N> view_ConstStockListND(const Rcpp::List& L, int st,
                                                 const std::string& prefix) { 
  return as_ConstArrayViewND<N>(static_cast<SEXP>(L[st]),
                                prefix + "_" + std::to_string(st));
} 

inline ConstArrayView3D view_ConstStockList3D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<3>(L, st, "StockList3D");
} 

inline ConstArrayView4D view_ConstStockList4D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<4>(L, st, "StockList4D");
} 

inline ConstArrayView5D view_ConstStockList5D(const Rcpp::List& L, int st) {
  return view_ConstStockListND<5>(L, st, "StockList5D");
} 

inline ArrayView3D view_StockList3D(Rcpp::List& L, int st) {
  return as_ArrayViewND<3>(static_cast<SEXP>(L[st]),
                           "StockList3D_" + std::to_string(st));
}

inline ArrayView4D view_StockList4D(Rcpp::List& L, int st) {
  return as_ArrayViewND<4>(static_cast<SEXP>(L[st]),
                           "StockList4D_" + std::to_string(st));
}

inline ArrayView5D view_StockList5D(Rcpp::List& L, int st) {
  return as_ArrayViewND<5>(static_cast<SEXP>(L[st]),
                           "StockList5D_" + std::to_string(st));
}

#endif