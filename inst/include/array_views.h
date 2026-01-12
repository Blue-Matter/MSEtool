#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include <array>
#include <algorithm>
#include "array_types.h"   // Array2D ... Array5D

#ifndef MSETOOL_ARRAY_VIEWS_H
#define MSETOOL_ARRAY_VIEWS_H

// ---------------------------------------------------------
// Forward declarations (from array_nd.h)
// ---------------------------------------------------------

template <size_t N>
struct ArrayND;

// Aliases (should be in array_types.h, but safe here)
using Array2D = ArrayND<2>;
using Array3D = ArrayND<3>;
using Array4D = ArrayND<4>;
using Array5D = ArrayND<5>;

// ---------------------------------------------------------
// Dimension position helpers (recommended)
// ---------------------------------------------------------

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

// ---------------------------------------------------------
// Safe ArrayND construction from R objects
// ---------------------------------------------------------

inline Array2D as_Array2D(const Rcpp::NumericVector& x) {
  auto d = x.attr("dim");
  if (d.size() != 2) Rcpp::stop("Expected 2D array");
  return Array2D(x, d[0], d[1]);
}

inline Array3D as_Array3D(const Rcpp::NumericVector& x) {
  auto d = x.attr("dim");
  if (d.size() != 3) Rcpp::stop("Expected 3D array");
  return Array3D(x, d[0], d[1], d[2]);
}

inline Array4D as_Array4D(const Rcpp::NumericVector& x) {
  auto d = x.attr("dim");
  if (d.size() != 4) Rcpp::stop("Expected 4D array");
  return Array4D(x, d[0], d[1], d[2], d[3]);
}

inline Array5D as_Array5D(const Rcpp::NumericVector& x) {
  auto d = x.attr("dim");
  if (d.size() != 5) Rcpp::stop("Expected 5D array");
  return Array5D(x, d[0], d[1], d[2], d[3], d[4]);
}

// ---------------------------------------------------------
// Slice helpers (views with copied data, explicit semantics)
// ---------------------------------------------------------

inline Array3D slice_year(const Array4D& x, int year) {
  Array3D out(x.dim[0], x.dim[1], x.dim[3]);
  for (int i = 0; i < x.dim[0]; ++i)
    for (int j = 0; j < x.dim[1]; ++j)
      for (int k = 0; k < x.dim[3]; ++k)
        out(i,j,k) = x(i,j,year,k);
  return out;
}

inline Array4D slice_year(const Array5D& x, int year) {
  Array4D out(x.dim[0], x.dim[1], x.dim[3], x.dim[4]);
  for (int i = 0; i < x.dim[0]; ++i)
    for (int j = 0; j < x.dim[1]; ++j)
      for (int k = 0; k < x.dim[3]; ++k)
        for (int l = 0; l < x.dim[4]; ++l)
          out(i,j,k,l) = x(i,j,year,k,l);
  return out;
}

// ---------------------------------------------------------
// Broadcasting-aware generic assignment: nD → ND
// ---------------------------------------------------------

template <size_t N_big, size_t N_small>
inline void assign_nd_into_Nd(
    ArrayND<N_big>& target,
    const ArrayND<N_small>& src,
    const std::array<size_t, N_small>& map,   // src dim → target dim
    const std::array<int, N_big>& fixed        // -1 = free, else fixed index
) {
  static_assert(N_small < N_big, "Source must have fewer dimensions");
  
  // Validate compatibility
  for (size_t i = 0; i < N_small; ++i) {
    int td = target.dim[ map[i] ];
    int sd = src.dim[i];
    if (sd != 1 && sd != td) {
      Rcpp::stop(
        "assign_nd_into_Nd: incompatible dimension (src=%d, target=%d)",
        sd, td
      );
    }
  }
  
  std::array<int, N_big> idx_big{};
  std::array<int, N_small> idx_small{};
  
  for (int flat = 0; flat < target.size(); ++flat) {
    
    // unravel flat index
    int tmp = flat;
    for (size_t d = 0; d < N_big; ++d) {
      idx_big[d] = tmp % target.dim[d];
      tmp /= target.dim[d];
    }
    
    // fixed dimension check
    bool ok = true;
    for (size_t d = 0; d < N_big; ++d) {
      if (fixed[d] >= 0 && idx_big[d] != fixed[d]) {
        ok = false;
        break;
      }
    }
    if (!ok) continue;
    
    // map + broadcast
    for (size_t i = 0; i < N_small; ++i) {
      int t = idx_big[ map[i] ];
      idx_small[i] = (src.dim[i] == 1 ? 0 : t);
    }
    
    target.x[flat] = src(idx_small);
  }
}

// ---------------------------------------------------------
// Convenience wrappers (common OM cases)
// ---------------------------------------------------------

inline void assign_3d_into_5d(
    Array5D& target,
    const Array3D& src,
    int stock,
    int year
) {
  assign_nd_into_Nd<5,3>(
      target,
      src,
      /* map   */ { Dim5::sim, Dim5::fleet, Dim5::area },
      /* fixed */ { -1, stock, year, -1, -1 }
  );
}

inline void assign_4d_into_5d(
    Array5D& target,
    const Array4D& src,
    int year
) {
  assign_nd_into_Nd<5,4>(
      target,
      src,
      /* map   */ { Dim5::sim, Dim5::stock, Dim5::fleet, Dim5::area },
      /* fixed */ { -1, -1, year, -1, -1 }
  );
}

#endif

