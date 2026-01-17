#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include <array>
#include <algorithm>
#include "array_types.h"   
#include "array_nd.h"   

template <size_t N>
struct ArrayND;

using Array1D = ArrayND<1>;
using Array2D = ArrayND<2>;
using Array3D = ArrayND<3>;
using Array4D = ArrayND<4>;
using Array5D = ArrayND<5>;

// Dimension position helpers 
// Update values if dimension position changes 
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
inline ArrayND<N> as_ArrayND(const Rcpp::NumericVector& x) {
  
  if (!x.hasAttribute("dim")) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) {
    dim[i] = d[i];
  }
   
  return ArrayND<N>(x, dim);
}
 

// Slice helpers 
inline Array2D 
slice_year(const Array3D& x, int year) {
  std::array<int, 2> dim = {x.dim[0], x.dim[1]};
  Array2D out(dim);
  for (int i = 0; i < x.dim[0]; ++i) {
    for (int j = 0; j < x.dim[1]; ++j) {
      out(i, j) = x(i, j, year);
    }
  }
  return out;
}

inline Array3D 
slice_year(const Array4D& x, int year) {
  
  std::array<int, 3> dim = {
    x.dim[0], x.dim[1], x.dim[3]
    };
  Array3D out(dim);
  for (int i = 0; i < x.dim[0]; ++i) {
    for (int j = 0; j < x.dim[1]; ++j) {
      for (int k = 0; k < x.dim[3]; ++k) {
        out(i, j, k) = x(i, j, year, k);
      }
    }
  }
  return out;
}

inline Array4D 
slice_year(const Array5D& x, int year) {
  std::array<int, 4> dim = {
    x.dim[0], x.dim[1], x.dim[3], x.dim[4]
  };
  
  Array4D out(dim);
  for (int i = 0; i < x.dim[0]; ++i) {
    for (int j = 0; j < x.dim[1]; ++j) {
      for (int k = 0; k < x.dim[3]; ++k) {
        for (int l = 0; l < x.dim[4]; ++l) {
          out(i, j, k, l) = x(i, j, year, k, l);
        } 
      }
    }
  }
  return out;
} 

// generic assignment nD → ND with broadcasting 
template <size_t N_big, size_t N_small>
inline void assign_nd_into_Nd(
    ArrayND<N_big>& target,
    const ArrayND<N_small>& src,
    const std::array<size_t, N_small>& map,   
    const std::array<int, N_big>& fixed) {
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

// Assignment wrappers



// 3D sim, fleet, area into 5D sim, stock, year, fleet, area
inline void sFA_into_sSYFA(
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

// 3D sim, fleet, area into 4D sim, year, fleet, area
inline void sFA_into_sYFA(
    Array4D& target,
    const Array3D& src,
    int year
) {
  assign_nd_into_Nd<4,3>(
      target,
      src,
      /* map   */ { Dim4::sim, Dim4::fleet, Dim4::area },
      /* fixed */ { -1, year, -1, -1 }
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

// Array views
template <size_t N>
struct ArrayViewND;

template <size_t N>
struct ConstArrayViewND;

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
  
  ArrayViewND() : x(nullptr) { dim.fill(0); }
   
  ArrayViewND(double* ptr,
              const std::array<int, N>& dim_)
    : x(ptr), dim(dim_) {}
   
  // Element access (multi-index)
  inline double& operator()(const std::array<int, N>& idx) {
    int flat = 0;
    int stride = 1;
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
  
  inline double operator()(const std::array<int, N>& idx) const {
    int flat = 0;
    int stride = 1;
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

template <size_t N>
struct ConstArrayViewND {
  
  const double* x;
  std::array<int, N> dim;
  
  // default constructor (null view)
  ConstArrayViewND()
    : x(nullptr)   {
    dim.fill(0);
  }
  
  ConstArrayViewND(const double* ptr,
                   const std::array<int, N>& dim_)
    : x(ptr), dim(dim_) {}
   
  inline double operator()(const std::array<int, N>& idx) const {
    int flat = 0;
    int stride = 1;
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
inline ArrayViewND<N>
as_ArrayViewND(Rcpp::NumericVector& x) {
  
  if (!x.hasAttribute("dim")) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) {
    dim[i] = d[i];
  }
   
  return ArrayViewND<N>(REAL(x), dim);
} 

template <size_t N>
inline ConstArrayViewND<N>
as_ConstArrayViewND(const Rcpp::NumericVector& x) {
   
  if (!x.hasAttribute("dim")) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  Rcpp::IntegerVector d = x.attr("dim");
  if (d.size() != static_cast<int>(N)) {
    Rcpp::stop("Expected %dD array", N);
  }
   
  std::array<int, N> dim;
  for (size_t i = 0; i < N; ++i) {
    dim[i] = d[i];
  }
   
  return ConstArrayViewND<N>(REAL(x), dim);
}


// Slice sim dimension 
template <size_t N>
inline ArrayND<N-1>
slice_sim(const ArrayND<N>& x, int sim) {
  static_assert(N >= 2, "slice_sim requires N >= 2");
  
  if (sim < 0 || sim >= x.dim[0]) {
    Rcpp::stop("slice_sim: sim index out of bounds");
  }
   
  // output dimensions: drop dim[0]
  std::array<int, N-1> dim_out;
  for (size_t d = 1; d < N; ++d) {
    dim_out[d - 1] = x.dim[d];
  }
   
  ArrayND<N-1> out(dim_out);
   
  // copy
  std::array<int, N> idx_in{};
  std::array<int, N-1> idx_out{};
   
  idx_in[0] = sim;
   
  for (int flat = 0; flat < out.size(); ++flat) {
     
    // unravel flat index in out
    int tmp = flat;
    for (size_t d = 0; d < N-1; ++d) {
      idx_out[d] = tmp % dim_out[d];
      tmp /= dim_out[d];
      idx_in[d + 1] = idx_out[d];
    }
     
    out.x[flat] = x(idx_in);
  }
   
  return out;
}

template <size_t N>
inline ArrayViewND<N-1>
slice_sim_view(ArrayViewND<N>& x, int sim) {
  static_assert(N >= 2, "slice_sim_view requires N >= 2");
  
  if (sim < 0 || sim >= x.dim[0]) {
    Rcpp::stop("slice_sim_view: sim index out of bounds");
  }
   
  // output dimensions (drop sim)
  std::array<int, N-1> dim_out;
  for (size_t d = 1; d < N; ++d) {
    dim_out[d - 1] = x.dim[d];
  }
   
  // column-major: sim is stride-1
  double* ptr = x.x + sim;
   
  return ArrayViewND<N-1>(ptr, dim_out);
} 


template <size_t N>
inline ArrayND<N-1>
slice_sim_broadcast(const ArrayND<N>& x, int simin) {
  static_assert(N >= 2, "slice_sim requires N >= 2");
  
  const int sim = (x.dim[0] == 1 ? 0 : simin < x.dim[0] ? simin : -1);
  
  // output dimensions: drop dim[0]
  std::array<int, N-1> dim_out;
  for (size_t d = 1; d < N; ++d) {
    dim_out[d - 1] = x.dim[d];
  }
   
  ArrayND<N-1> out(dim_out);
   
  // copy
  std::array<int, N> idx_in{};
  std::array<int, N-1> idx_out{};
   
  idx_in[0] = sim;
  
  for (int flat = 0; flat < out.size(); ++flat) {
    
    // unravel flat index in out
    int tmp = flat;
    for (size_t d = 0; d < N-1; ++d) {
      idx_out[d] = tmp % dim_out[d];
      tmp /= dim_out[d];
      idx_in[d + 1] = idx_out[d];
    }
     
    out.x[flat] = x(idx_in);
  }
   
  return out;
} 

template <size_t N>
inline ConstArrayViewND<N-1>
slice_sim_view_broadcast(const ConstArrayViewND<N>& x, int simin) {
  static_assert(N >= 2, "slice_sim_view_broadcast requires N >= 2");
  
  // broadcast sim dimension if needed
  const int sim =
    (x.dim[0] == 1 ? 0 :
       (simin >= 0 && simin < x.dim[0] ? simin : -1));
   
  if (sim < 0) {
    Rcpp::stop("slice_sim_view_broadcast: sim index out of bounds");
  }
   
  // output dimensions (drop sim)
  std::array<int, N-1> dim_out;
  for (size_t d = 1; d < N; ++d) {
    dim_out[d - 1] = x.dim[d];
  }
   
  // column-major offset: sim index is stride-1
  const double* ptr = x.x + sim;
   
  return ConstArrayViewND<N-1>(ptr, dim_out);
} 

inline ArrayND<3> copy_from_view(const ArrayViewND<3>& v) {
  ArrayND<3> out(v.dim);
  for (int i = 0; i < v.dim[0]; ++i)
    for (int j = 0; j < v.dim[1]; ++j)
      for (int k = 0; k < v.dim[2]; ++k)
        out(i, j, k) = v(i, j, k);
  return out;
}


#endif




