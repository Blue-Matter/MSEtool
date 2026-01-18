#ifndef MSEtool_ARRAY_ND_H
#define MSEtool_ARRAY_ND_H

#include <Rcpp.h>
#include <array>
#include <numeric>
#include <algorithm>


// Compile-time bounds control
// Define MSE_ARRAY_BOUNDS_CHECK in debug builds
//
#ifndef MSE_ARRAY_BOUNDS_CHECK
#define MSE_ARRAY_BOUNDS_CHECK 1
#endif

template <size_t N>
struct ArrayND {
  
  static_assert(N >= 1, "ArrayND requires N >= 1");
  
  std::array<int, N> dim; 
  Rcpp::NumericVector x;
  
  // Constructors
  ArrayND() {
    dim.fill(0);
  }

  // Allocate
  explicit ArrayND(const std::array<int, N>& d)
    : dim(d), x(size()) {}

    // Allocate + fill
  ArrayND(const std::array<int, N>& d, double value)
    : dim(d), x(size(), value) {}

  // Wrap existing vector with explicit dims
  ArrayND(const Rcpp::NumericVector& x_,
          const std::array<int, N>& d)
    : dim(d), x(x_) {
   
    if (x.size() != size()) {
      Rcpp::stop("ArrayND: vector length does not match dimensions");
    }
  }

  // Wrap existing R array (infer dims)
  explicit ArrayND(const Rcpp::NumericVector& x_)
    : x(x_) {
   
    if (!x.hasAttribute("dim")) {
      Rcpp::stop("ArrayND: object has no 'dim' attribute");
    }
     
    Rcpp::IntegerVector d = x.attr("dim");
     
    if (d.size() != static_cast<int>(N)) {
      Rcpp::stop("ArrayND: expected %dD array", N);
    }
     
    for (size_t i = 0; i < N; ++i) {
      dim[i] = d[i];
    } 
    
    if (x.size() != size()) {
      Rcpp::stop("ArrayND: vector length does not match dimensions");
    } 
  }
  
  
  // Size helpers
  inline int size() const {
    return std::accumulate(
      dim.begin(), dim.end(),
      1, std::multiplies<int>()
    );
  } 
  
  inline Rcpp::IntegerVector dim_r() const {
    Rcpp::IntegerVector out(N);
    for (size_t i = 0; i < N; ++i) {
      out[i] = dim[i];
    } 
    return out;
  } 
  
  
  // Broadcasting logic (dim 0 only)
  inline int map0(int i) const {
    return (dim[0] == 1 ? 0 : i);
  } 
  
  
  // Indexing
  inline int idx(const std::array<int, N>& ind) const {
    int offset = map0(ind[0]);
    int stride = dim[0];
     
    for (size_t k = 1; k < N; ++k) {
      offset += ind[k] * stride;
      stride *= dim[k];
    } 
    return offset;
  } 
  
#if MSE_ARRAY_BOUNDS_CHECK
  inline void check_bounds(const std::array<int, N>& ind) const {
     
    const int i0 = map0(ind[0]);
    if (i0 < 0 || i0 >= dim[0]) {
      Rcpp::stop("ArrayND index out of bounds");
    } 
    
    for (size_t k = 1; k < N; ++k) {
      if (ind[k] < 0 || ind[k] >= dim[k]) {
        Rcpp::stop("ArrayND index out of bounds");
      }
    }
  }
#endif
  
  template <typename... Args> 
  inline double& operator()(Args... args) {
    static_assert(sizeof...(Args) == N,
                  "Incorrect number of indices");
    std::array<int, N> ind{ static_cast<int>(args)... };
    
#if MSE_ARRAY_BOUNDS_CHECK
    check_bounds(ind);
#endif
    return x[idx(ind)];
  }
  
  template <typename... Args>
  inline double operator()(Args... args) const {
    static_assert(sizeof...(Args) == N,
                  "Incorrect number of indices");
    
    std::array<int, N> ind{ static_cast<int>(args)... };
    
#if MSE_ARRAY_BOUNDS_CHECK
    check_bounds(ind);
#endif
    
    return x[idx(ind)];
  } 

  
  // Array-based indexing
  inline double& operator()(const std::array<int, N>& ind) {
#if MSE_ARRAY_BOUNDS_CHECK
    check_bounds(ind);
#endif
    return x[idx(ind)];
  }
  
  inline double operator()(const std::array<int, N>& ind) const {
#if MSE_ARRAY_BOUNDS_CHECK
    check_bounds(ind);
#endif
    return x[idx(ind)];
  }
  

  // Slice helpers
  
  // Fix dimension K at index `fixed`
  template <size_t K>
  ArrayND<N - 1> slice(int fixed) const {
    static_assert(K < N, "Invalid slice dimension");
    
    std::array<int, N - 1> newdim;
    for (size_t i = 0, j = 0; i < N; ++i) {
      if (i != K) newdim[j++] = dim[i];
    }
    
    ArrayND<N - 1> out(newdim);

    std::array<int, N> ind{};
    for (int flat = 0; flat < out.size(); ++flat) {
      int tmp = flat;
      for (size_t i = 0; i < N; ++i) {
        if (i == K) {
          ind[i] = fixed;
        } else {
          ind[i] = tmp % dim[i];
          tmp /= dim[i];
        }
      }
      const int off = idx(ind);
      if (off < 0 || off >= x.size())
        Rcpp::stop("slice<K>: idx out of bounds");
      out.x[flat] = x[off];
    }
    
    return out;
  }
  
  void set_slice(int sim, const ArrayND<N-1>& src);
};


// set sim slice
template <size_t N>
inline void ArrayND<N>::set_slice(
    int sim,
    const ArrayND<N-1>& src
) {
  static_assert(N >= 2, "set_slice requires N >= 2");
  
  if (sim < 0 || sim >= dim[0]) {
    Rcpp::stop("set_slice: sim index out of bounds");
  }
  
  // Check dimensions
  for (size_t d = 1; d < N; ++d) {
    if (dim[d] != src.dim[d - 1]) {
      Rcpp::stop("set_slice: dimension mismatch at dim %d (target=%d, src=%d)",
                 d, dim[d], src.dim[d - 1]);
    }
  } 
  std::array<int, N> idx_big{};
  std::array<int, N-1> idx_small{};
  idx_big[0] = sim;
   
  for (int flat = 0; flat < src.size(); ++flat) {
     
    // unravel flat index in src
    int tmp = flat;
    for (size_t d = 0; d < N-1; ++d) {
      idx_small[d] = tmp % src.dim[d];
      tmp /= src.dim[d];
      idx_big[d + 1] = idx_small[d];
    }
     
    (*this)(idx_big) = src.x[flat];
  } 
} 

inline void set_slice_age(
    ArrayND<4>& target,   // [sim, age, year, area]
    int sim,
    const ArrayND<3>& src // [age, year, area]
) {
  if (sim < 0 || sim >= target.dim[0]) {
    Rcpp::stop("set_slice_age: sim index out of bounds");
  }
  
  // semantic checks
  if (target.dim[1] != src.dim[0]) {
    Rcpp::stop("set_slice_age: age mismatch (target=%d, src=%d)",
               target.dim[1], src.dim[0]);
  }
  
  if (target.dim[2] != src.dim[1]) {
    Rcpp::stop("set_slice_age: year mismatch");
  }
  
  if (target.dim[3] != src.dim[2]) {
    Rcpp::stop("set_slice_age: area mismatch");
  }
  std::array<int,4> I4{};
  std::array<int,3> I3{};
  I4[0] = sim;

  for (int a = 0; a < src.dim[0]; ++a) {
    I4[1] = a; I3[0] = a;
    for (int y = 0; y < src.dim[1]; ++y) {
      I4[2] = y; I3[1] = y;
      for (int ar = 0; ar < src.dim[2]; ++ar) {
        I4[3] = ar; I3[2] = ar;
        target(I4) = src(I3);
      }
    }
  }
} 

inline void set_slice_age_fleet(
    ArrayND<5>& target,   // [sim, age, year, fleet, area]
    int sim,
    const ArrayND<4>& src // [age, year, fleet, area]
) {
  if (sim < 0 || sim >= target.dim[0])
    Rcpp::stop("set_slice_age_fleet: sim out of bounds");
  
  if (target.dim[1] != src.dim[0]) Rcpp::stop("age mismatch");
  if (target.dim[2] != src.dim[1]) Rcpp::stop("year mismatch");
  if (target.dim[3] != src.dim[2]) Rcpp::stop("fleet mismatch");
  if (target.dim[4] != src.dim[3]) Rcpp::stop("area mismatch");
   
  for (int a = 0; a < src.dim[0]; ++a)
    for (int y = 0; y < src.dim[1]; ++y)
      for (int f = 0; f < src.dim[2]; ++f)
        for (int ar = 0; ar < src.dim[3]; ++ar)
          target(sim, a, y, f, ar) = src(a, y, f, ar);
} 


inline void set_slice_length(
    ArrayND<4>& target,   // [sim, length, year, area]
    int sim,
    const ArrayND<3>& src // [length, year, area]
) {
  if (sim < 0 || sim >= target.dim[0])
    Rcpp::stop("set_slice_length: sim out of bounds");
  
  if (target.dim[1] != src.dim[0])
    Rcpp::stop("set_slice_length: length mismatch (target=%d, src=%d)",
               target.dim[1], src.dim[0]);
  
  if (target.dim[2] != src.dim[1])
    Rcpp::stop("set_slice_length: year mismatch");
  if (target.dim[3] != src.dim[2])
    Rcpp::stop("set_slice_length: area mismatch");

  for (int l = 0; l < src.dim[0]; ++l)
    for (int y = 0; y < src.dim[1]; ++y)
      for (int a = 0; a < src.dim[2]; ++a)
        target(sim, l, y, a) = src(l, y, a);
} 


#endif // MSEtool_ARRAY_ND_H
 