#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include <array>
#include <algorithm>
#include "array_types.h"   
#include "array_nd.h"   

template <size_t N>
struct ArrayND;

using Array2D = ArrayND<2>;
using Array3D = ArrayND<3>;
using Array4D = ArrayND<4>;
using Array5D = ArrayND<5>;

// Dimension position helpers 
namespace Dim2 {
constexpr size_t i = 0;
constexpr size_t j = 1;
}

namespace Dim3 {
constexpr size_t sim  = 0;
constexpr size_t age  = 1;
constexpr size_t area = 2;
}

namespace Dim4 {
constexpr size_t sim   = 0;
constexpr size_t age   = 1;
constexpr size_t fleet = 2;
constexpr size_t area  = 3;
}

namespace Dim5 {
constexpr size_t sim   = 0;
constexpr size_t stock = 1;
constexpr size_t year  = 2;
constexpr size_t fleet = 3;
constexpr size_t area  = 4;
}

// ArrayND construction from R objects
// Generic Constructor
template <size_t N>
inline ArrayND<N> as_ArrayND(const Rcpp::NumericVector& x, const std::string& name = "") {
  if (!x.hasAttribute("dim")) {
    Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  }
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) {
    Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  }

  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) dim[i] = d[i];

  return ArrayND<N>(x, dim, name);
} 


// // Slice helpers 
// inline Array2D 
// slice_year(const Array3D& x, int year) {
//   std::array<int, 2> dim = {x.dim[0], x.dim[1]};
//   Array2D out(dim);
//   for (int i = 0; i < x.dim[0]; ++i) {
//     for (int j = 0; j < x.dim[1]; ++j) {
//       out(i, j) = x(i, j, year);
//     }
//   }
//   return out;
// }
// 
// inline Array3D 
// slice_year(const Array4D& x, int year) {
//   
//   std::array<int, 3> dim = {
//     x.dim[0], x.dim[1], x.dim[3]
//   };
//   Array3D out(dim);
//   for (int i = 0; i < x.dim[0]; ++i) {
//     for (int j = 0; j < x.dim[1]; ++j) {
//       for (int k = 0; k < x.dim[3]; ++k) {
//         out(i, j, k) = x(i, j, year, k);
//       }
//     }
//   }
//   return out;
// }
// 
// inline Array4D 
// slice_year(const Array5D& x, int year) {
//   std::array<int, 4> dim = {
//     x.dim[0], x.dim[1], x.dim[3], x.dim[4]
//   };
//   
//   Array4D out(dim);
//   for (int i = 0; i < x.dim[0]; ++i) {
//     for (int j = 0; j < x.dim[1]; ++j) {
//       for (int k = 0; k < x.dim[3]; ++k) {
//         for (int l = 0; l < x.dim[4]; ++l) {
//           out(i, j, k, l) = x(i, j, year, k, l);
//         } 
//       }
//     }
//   }
//   return out;
// } 


// Array views
template <size_t N>
struct ArrayViewND;

template <size_t N>
struct ConstArrayViewND;

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


template <size_t N>
struct ArrayViewND {
  double* x;
  std::array<int, N> dim;
  std::string name;
  
  ArrayViewND(double* ptr, const std::array<int, N>& dim_, const std::string& nm = "")
    : x(ptr), dim(dim_), name(nm) {}

  // Multi-index access
  inline double& operator()(const std::array<int, N>& idx) {
    int flat = 0, stride = 1;
    for (size_t d = 0; d < N; ++d) {
      flat += idx[d] * stride;
      stride *= dim[d];
    } 
    return x[flat];
  }

  template <typename... Args>
  inline double& operator()(Args... args) {
    static_assert(sizeof...(Args) == N, "Invalid number of indices");
    std::array<int, N> idx{ static_cast<int>(args)... };
    return (*this)(idx);
  }
};  

template <size_t N>
struct ConstArrayViewND {
  const double* x;
  std::array<int, N> dim;
  std::string name;
  
  ConstArrayViewND(const double* ptr, const std::array<int, N>& dim_, const std::string& nm = "")
    : x(ptr), dim(dim_), name(nm) {}
   
  inline double operator()(const std::array<int, N>& idx) const {
    int flat = 0, stride = 1;
    for (size_t d = 0; d < N; ++d) {
      flat += idx[d] * stride;
      stride *= dim[d];
    }
    return x[flat];
  }

  template <typename... Args>
  inline double operator()(Args... args) const {
    static_assert(sizeof...(Args) == N, "Invalid number of indices");
    std::array<int, N> idx{ static_cast<int>(args)... };
    return (*this)(idx);
  }
}; 



// ArrayView constructors from R objects
template <size_t N>
inline ArrayViewND<N> as_ArrayViewND(Rcpp::NumericVector& x, const std::string& name = "") {
  if (!x.hasAttribute("dim")) Rcpp::stop("ArrayViewND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) Rcpp::stop("ArrayViewND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) dim[i] = d[i];
  return ArrayViewND<N>(REAL(x), dim, name);
}

template <size_t N>
inline ConstArrayViewND<N> as_ConstArrayViewND(const Rcpp::NumericVector& x, const std::string& name = "") {
  if (!x.hasAttribute("dim")) Rcpp::stop("ConstArrayViewND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) Rcpp::stop("ConstArrayViewND (" + (name.empty() ? "unnamed" : name) + "): expected " + std::to_string(N) + "D array");
  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) dim[i] = d[i];
  return ConstArrayViewND<N>(REAL(x), dim, name);
} 



// Non-mutable S4 slot arrays
template <size_t N>
inline ConstArrayViewND<N> GetMisc_ConstArrayView(Rcpp::S4& obj, const char* nm) {
  const Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(nm)) Rcpp::stop("Hist@Misc$" + std::string(nm) + " not found");
  Rcpp::NumericVector src = Misc[nm];
  if (!src.hasAttribute("dim")) Rcpp::stop("Hist@Misc$" + std::string(nm) + " has no dim attribute");
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != N) Rcpp::stop("Hist@Misc$" + std::string(nm) + " is not a " + std::to_string(N) + "D array");
  return as_ConstArrayViewND<N>(src, nm);
}



// StockList Views
inline ArrayView3D view_StockList3D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<3>(x, "StockList3D_" + std::to_string(st));
}
inline ConstArrayView3D view_ConstStockList3D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<3>(x, "StockList3D_" + std::to_string(st));
} 

inline ArrayView4D view_StockList4D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<4>(x, "StockList4D_" + std::to_string(st));
}

inline ConstArrayView4D view_ConstStockList4D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<4>(x, "StockList4D_" + std::to_string(st));
} 

inline ArrayView5D view_StockList5D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<5>(x, "StockList5D_" + std::to_string(st));
}

inline ConstArrayView5D view_ConstStockList5D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<5>(x, "StockList5D_" + std::to_string(st));
}

#endif
