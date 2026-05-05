#ifndef MSEtool_ARRAY_ND_H
#define MSEtool_ARRAY_ND_H

#include <Rcpp.h>
#include <array>
#include <numeric>
#include <algorithm>
#include <string>

template <size_t N>
struct ArrayND {
  
  static_assert(N >= 2, "ArrayND requires N >= 2");
  
  std::array<int, N> dim; 
  Rcpp::NumericVector x;
  std::string name;  
  
  ArrayND() { dim.fill(0); }
  
  explicit ArrayND(const std::array<int, N>& d, const std::string& nm = "")
    : dim(d), x(size()), name(nm) {}
  
  ArrayND(const std::array<int, N>& d, double value, const std::string& nm = "")
    : dim(d), x(size(), value), name(nm) {}
   
  ArrayND(const Rcpp::NumericVector& x_,
          const std::array<int, N>& d,
          const std::string& nm = "")
    : dim(d), x(x_), name(nm) {
    if (x.size() != size())
      Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) +
        "): vector length does not match dimensions");
  }
   
  ArrayND(SEXP sexp, const std::array<int, N>& d, const std::string& nm = "")
    : dim(d), x(sexp), name(nm) {  
    if (x.size() != size())
      Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) +
        "): vector length does not match dimensions");
  }
  
  inline int size() const {
    return std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<int>());
  }

  inline Rcpp::IntegerVector dim_r() const {
    Rcpp::IntegerVector out(N);
    for (size_t i = 0; i < N; ++i) out[i] = dim[i];
    return out;
  }
  
  inline int map0(int i) const { return (dim[0] == 1 ? 0 : i); }
   
  inline int idx(const std::array<int, N>& ind) const {
    int offset = map0(ind[0]);
    int stride = dim[0];
    for (size_t k = 1; k < N; ++k) {
      offset += ind[k] * stride;
      stride *= dim[k];
    } 
    return offset;
  }
   
  inline void check_bounds(const std::array<int, N>& ind) const {
    int i0 = map0(ind[0]);
    if (i0 < 0 || i0 >= dim[0])
      Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) +
        ") index out of bounds at dim0: " + std::to_string(i0) +
        " >= " + std::to_string(dim[0]));
    for (size_t k = 1; k < N; ++k) {
      if (ind[k] < 0 || ind[k] >= dim[k]) {
        std::string idx_str;
        for (size_t j = 0; j < N; ++j) {
          idx_str += std::to_string(ind[j]);
          if (j != N-1) idx_str += ",";
        } 
        Rcpp::stop("ArrayND (" + (name.empty() ? "unnamed" : name) +
          ") index out of bounds at dim" + std::to_string(k) +
          ": " + std::to_string(ind[k]) + " >= " + std::to_string(dim[k]) +
          " (indices [" + idx_str + "])");
      }
    }
  }
   
  template <typename... Args>
  inline double& operator()(Args... args) {
    static_assert(sizeof...(Args) == N, "Incorrect number of indices");
    std::array<int, N> ind{ static_cast<int>(args)... };
    check_bounds(ind);
    return x[idx(ind)];
  }
   
  template <typename... Args>
  inline double operator()(Args... args) const {
    static_assert(sizeof...(Args) == N, "Incorrect number of indices");
    std::array<int, N> ind{ static_cast<int>(args)... };
    check_bounds(ind);
    return x[idx(ind)];
  }
   
  inline double& operator()(const std::array<int, N>& ind) {
    check_bounds(ind);
    return x[idx(ind)];
  }
   
  inline double operator()(const std::array<int, N>& ind) const {
    check_bounds(ind);
    return x[idx(ind)];
  }
}; 

#endif
