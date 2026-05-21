#ifndef MSEtool_ARRAY_ND_H
#define MSEtool_ARRAY_ND_H

#include <Rcpp.h>
#include <array>
#include <numeric>
#include <algorithm>

template <size_t N>
struct ArrayND {
  
  static_assert(N >= 2, "ArrayND requires N >= 2");
  
  std::array<int, N> dim;
  std::array<int, N> strides;   // pre-computed, eliminates per-call multiply loop
  Rcpp::NumericVector x;
  
  ArrayND() { dim.fill(0); strides.fill(0); }
  
  explicit ArrayND(const std::array<int, N>& d)
    : dim(d), x(size()) { compute_strides(); }
  
  ArrayND(const std::array<int, N>& d, double value)
    : dim(d), x(size(), value) { compute_strides(); }
  
  ArrayND(const Rcpp::NumericVector& x_, const std::array<int, N>& d)
    : dim(d), x(x_) {
    if (x.size() != size())
      Rcpp::stop("ArrayND: vector length does not match dimensions");
    compute_strides();
  }
  
  ArrayND(SEXP sexp, const std::array<int, N>& d)
    : dim(d), x(sexp) {
    if (x.size() != size())
      Rcpp::stop("ArrayND: vector length does not match dimensions");
    compute_strides();
  }
  
  inline int size() const {
    return std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<int>());
  }
  
  inline Rcpp::IntegerVector dim_r() const {
    Rcpp::IntegerVector out(N);
    for (size_t i = 0; i < N; ++i) out[i] = dim[i];
    return out;
  }
  
  // sim-broadcast: dim[0]==1 means all sims share the same slice
  inline int map0(int i) const { return (dim[0] == 1 ? 0 : i); }
  
  inline int idx(const std::array<int, N>& ind) const {
    int offset = map0(ind[0]);                  // handles sim broadcast
    for (size_t k = 1; k < N; ++k)
      offset += ind[k] * strides[k];
    return offset;
  }
  
  // --- mutable accessors ---
  
  template <typename... Args>
  inline double& operator()(Args... args) {
    static_assert(sizeof...(Args) == N, "Incorrect number of indices");
    return x[idx({ static_cast<int>(args)... })];
  }
  
  inline double& operator()(const std::array<int, N>& ind) {
    return x[idx(ind)];
  }
  
  // --- const accessors ---
  
  template <typename... Args>
  [[nodiscard]] inline double operator()(Args... args) const {
    static_assert(sizeof...(Args) == N, "Incorrect number of indices");
    return x[idx({ static_cast<int>(args)... })];
  }
  
  [[nodiscard]] inline double operator()(const std::array<int, N>& ind) const {
    return x[idx(ind)];
  }
  
private:
  inline void compute_strides() {
    strides[0] = 1;                             // stride[0] is implicit in map0
    for (size_t d = 1; d < N; ++d)
      strides[d] = strides[d-1] * dim[d-1];
  }
};

#endif