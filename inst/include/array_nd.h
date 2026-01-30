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
  
  static_assert(N >= 2, "ArrayND requires N >= 2");
  
  std::array<int, N> dim; 
  Rcpp::NumericVector x;
  
  // Constructors
  
  // Default
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
      Rcpp::stop("ArrayND: expected " + std::to_string(N) + "D array");
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
      if (i != K) {
        newdim[j++] = dim[i];
      } 
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
      out.x[flat] = x[idx(ind)];
    }
    return out;
  }
};

#endif // MSEtool_ARRAY_ND_H
