#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
#include <vector>
#include "array_types.h"
#include "array_views.h"


// Calculate the time step index
inline std::vector<int>
  CalcTSIndex(const Rcpp::NumericVector& Years,
              const Rcpp::NumericVector& YearsAll) {
    Rcpp::IntegerVector matchTS = Rcpp::match(Years, YearsAll);
    const int nTS = Years.size();
    
    std::vector<int> ts_index(nTS);
    
    for (int ts = 0; ts < nTS; ++ts) {
      if (matchTS[ts] == NA_INTEGER) {
        Rcpp::stop("Year not found in `YearsAll`");
      }
      ts_index[ts] = matchTS[ts] - 1; 
    }
    
    return ts_index;
  }


// Extract S4 slots into mutable Array
inline Array2D
Slot2Array2D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot);
  return as_ArrayND<2>(x);   // modifies Hist@slot in place
}

inline Array3D
Slot2Array3D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<3>(x);   
}

inline Array4D
Slot2Array4D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<4>(x);   
}

inline Array5D
Slot2Array5D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<5>(x);   
}


// Broadcast dimension i  
template <size_t N, class ArrayType>
inline int broadcast_dim(int x, const ArrayType& arr, int i) {
  static_assert(N > 0, "broadcast_dim: array must have at least one dimension (N>0)");
  
  if (i < 0 || i >= static_cast<int>(N)) {
    Rcpp::stop("broadcast_dim(): dimension index out of bounds");
  }
  
  const int d = arr.dim[i];

  if (d == 1)      return 0;
  if (x < d)       return x;

  Rcpp::stop("broadcast_dim(): dimension length incompatible with index");
}


template <size_t N, class ArrayType>
inline int sim_index(int sim, const ArrayType& arr, const char* name) {
  try {
    return broadcast_dim<N>(sim, arr, 0);
  } catch (...) {
    Rcpp::stop("sim_index(): incompatible sim dimension in " + std::string(name));
  }
} 


// Dimension checks 
template <size_t N, class ArrayType>
inline void check_dims(const ArrayType& arr,
                       const std::array<int, N>& expected,
                       const char* name,
                       int y = -1,
                       int t_ind = -1) {
  static_assert(N > 0, "check_dims: array must have at least one dimension (N>0)");
  
  for (size_t i = 0; i < N; ++i) {
    if (i == 0 && (arr.dim[0] != 1 && arr.dim[0] != expected[0])) {
      Rcpp::stop(std::string(name) + ": dimension 0 (sim) mismatch");
    } 
    else if (i != 0 && arr.dim[i] != expected[i]) {
      Rcpp::stop(std::string(name) + ": dimension " + std::to_string(int(i)) + " mismatch");

    }
  }

  // Optional y-bound check for time dimension 
  if (y >= 0 && t_ind >= 0 && t_ind < static_cast<int>(N)) {
    if (y >= arr.dim[t_ind]) {
      Rcpp::stop(std::string(name) + ": y index out of bounds (dimension " + std::to_string(t_ind) + ")");
    }
  } 
} 


inline void check_years_argument(SEXP Years, const char* name) {
  if (!Rf_isNumeric(Years)) {
    Rcpp::stop(std::string("Argument `") + name + "` must be a numeric vector");
  }
  
  if (Rf_length(Years) == 0) {
    Rcpp::stop(std::string("Argument `") + name + "` cannot be empty");
  }
}

#endif // HELPERS_H
